package edu.umass.cs.iesl.paperheader.tagger

/**
 * @author Kate Silverstein 
 *         created on 3/9/15
 */

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.la._
import cc.factorie.optimize._
import cc.factorie.infer._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._

object LabelDomain extends CategoricalDomain[String]
abstract class HLabel(labelname: String) extends LabeledCategoricalVariable(labelname)
class HeaderLabel(labelname: String, val token: Token) extends HLabel(labelname) {
  def domain = LabelDomain
  def hasNext = token.hasNext && token.next != null
  def hasPrev = token.hasPrev && token.prev != null
  def next = token.next
  def prev = token.prev
}

object HeaderFeaturesDomain extends CategoricalDomain[String]
class HeaderFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = HeaderFeaturesDomain
  override def skipNonCategories = true
}

object SpanLabelDomain extends CategoricalDomain[String]
class SpanHeaderLabel(val span: HeaderSpan, initialValue: String) extends HLabel(initialValue) {
  def domain = SpanLabelDomain
}
class HeaderSpan(doc: Document, labelString: String, start: Int, length: Int) extends TokenSpan(doc.asSection, start, length) {
  val label = new SpanHeaderLabel(this, labelString)
  override def toString() = "HeaderSpan(" + label.categoryValue + ")"
}

class SimpleHeaderCRFModel extends TemplateModel with Parameters {
  // factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[HeaderLabel, HeaderFeatures] {
    factorName = "observation"
    val weights = Weights(new DenseTensor2(LabelDomain.size, HeaderFeaturesDomain.dimensionSize))
    def unroll1(label: HeaderLabel) = Factor(label, label.token.attr[HeaderFeatures])
    def unroll2(tf: HeaderFeatures) = Factor(tf.token.attr[HeaderLabel], tf)
  }
  // transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[HeaderLabel, HeaderLabel] {
    factorName = "markov"
    val weights = Weights(new DenseTensor2(LabelDomain.size, LabelDomain.size))
    def unroll1(label: HeaderLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[HeaderLabel], label) else Nil
    def unroll2(label: HeaderLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[HeaderLabel]) else Nil
  }
  this += localTemplate
  this += transitionTemplate
}

trait Results {
  def setToMax(variables: Seq[MutableDiscreteVar]): Unit = nodeMarginals.zip(variables).map({case (nm, v) => v.set(nm.maxIndex)(null)})
  def nodeMarginals: Array[DenseTensor1]
  def logZ: Double
}

case class ViterbiResults(mapScore: Double, mapValues: Array[Int], localScores: Seq[DenseTensor1]) extends Results {
  val nodeMarginals: Array[DenseTensor1] = {
    val domainSize = localScores(0).size
    mapValues.map(i => {val t = new DenseTensor1(domainSize); t(i) = 1.0; t})
  }
  def logZ: Double = mapScore
}

object InferenceHelper {
  lazy val allowedHeaderTags = HeaderTagDomain.categories
  // TODO constrain transitions?
  // TODO constrainLocal ?
  def getLocalScores(varying: Seq[DiscreteVar], theModel: SimpleHeaderCRFModel): Array[DenseTensor1] = {
    val obsWeights = theModel.localTemplate.weights.value
    val a = Array.fill[DenseTensor1](varying.size)(null)
    var i = 0
    while (i < varying.length) {
      val scores = (obsWeights * varying(i).asInstanceOf[HeaderLabel].token.attr[HeaderFeatures].value).asInstanceOf[DenseTensor1]
      a(i) = scores
      i += 1
    }
    // TODO constrain stuff?
    a
  }
  def getScores(variables: Seq[DiscreteVar], model: Model): ChainCliqueValues = {
    val m = model.asInstanceOf[SimpleHeaderCRFModel]
    val markovScoresT = m.transitionTemplate.weights.value
    val markovScores = (0 to variables.size).map(_ => markovScoresT.copy)
    val localScores = getLocalScores(variables, m)
    ChainCliqueValues(localScores, markovScores.map(_.asInstanceOf[DenseTensor2]))
  }
  def infer(variables: Seq[DiscreteVar], model: Model): BPSummary = {
    val scores = getScores(variables.toSeq, model)
    val res = ChainHelper.viterbiFast(scores)
    val result = ViterbiResults(res.mapScore, res.mapValues, res.scores.localValues)
    val theVars = variables
    new BPSummary(BPMaxProductRing) {
      _logZ = result.mapScore
      override def logZ: Double = result.mapScore
      override def setToMaximize(implicit d: DiffList): Unit = {
        val labels = theVars.map(x => { x.asInstanceOf[MutableDiscreteVar] }).toSeq
        result.setToMax(labels)
      }
    }
  }
}

class SimpleHeaderCRFTrainer {
  val model = new SimpleHeaderCRFModel
  def wordToFeatures(token: Token): Unit = {
    token.attr += new HeaderFeatures(token)
    val features = token.attr[HeaderFeatures]
    features += s"W=${token.string}"
    features += s"SHAPE=${cc.factorie.app.strings.stringShape(token.string, 2)}"
  }
  def train(trainDocs: Seq[Document], testDocs: Seq[Document]): Unit = {
    implicit val random = new scala.util.Random
    //add features
    trainDocs.flatMap(_.tokens).foreach(wordToFeatures)
    HeaderFeaturesDomain.freeze()
    testDocs.flatMap(_.tokens).foreach(wordToFeatures)
    println(s"feature domain size: ${HeaderFeaturesDomain.dimensionDomain.size}")
    val trainLabels = trainDocs.flatMap(_.tokens).map(_.attr[HeaderLabel])
    val testLabels = testDocs.flatMap(_.tokens).map(_.attr[HeaderLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocs; sentence <- td.sentences if sentence.length > 0) yield sentence.tokens.map(_.attr[HeaderLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val trainer = new BatchTrainer(model.parameters, new LBFGS with L2Regularization)
    trainer.trainFromExamples(examples)
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocs.foreach(process)
    testDocs.foreach(process)
    val eval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", LabelDomain, testLabels.toIndexedSeq)
    println(eval)
  }
  def process(doc: Document): Document = {
    if (doc.tokenCount == 0) return doc
    for (sentence <- doc.sentences if sentence.tokens.size > 0) {
      val vars = sentence.tokens.map(_.attr[HeaderLabel]).toSeq
      val sum = InferenceHelper.infer(vars, model)
      sum.setToMaximize(null)
    }
    doc
  }
}

object SimpleHeaderTrainer {
  def main(args: Array[String]): Unit = {
//    val opts = new HeaderTaggerOpts
//    opts.parse(args)
//    val (trainDocs, devDocs) = HeaderTaggerUtils.loadDevAndTrainData(opts.dataDir.value, opts.dataSet.value)
//    trainDocs.foreach(doc => {
//      doc.tokens.foreach(t => {
//        t.attr += new HeaderLabel(t.attr[LabeledBilouHeaderTag].categoryValue, t)
//      })
//    })
//    devDocs.foreach(doc => {
//      doc.tokens.foreach(t => {
//        t.attr += new HeaderLabel(t.attr[LabeledBilouHeaderTag].categoryValue, t)
//      })
//    })
//    val trainer = new SimpleHeaderCRFTrainer
//    trainer.train(trainDocs, devDocs)
  }
}