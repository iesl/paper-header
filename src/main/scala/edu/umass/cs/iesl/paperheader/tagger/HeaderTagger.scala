package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.util._
import cc.factorie.variable._
import cc.factorie.app.chain.{ChainModel, SegmentEvaluation}
import edu.umass.cs.iesl.paperheader.load._
import java.io._
import java.net.URL


/**
 * Created by kate on 9/25/14.
 */

//grobid: 'degree', 'pubnum', 'submission', 'reference', 'author', 'abstract', 'title', 'grant', 'phone', 'note', 'affiliation', 'intro', 'keyword', 'entitle', 'address', 'date', 'web', 'copyright', 'dedication', 'email'
object LabelDomain extends CategoricalDomain[String]

abstract class HLabel(labelname: String) extends LabeledCategoricalVariable(labelname)
class HeaderLabel(labelname: String, val token: Token) extends HLabel(labelname) {
  def domain = LabelDomain
}

object FeatureDomain extends CategoricalVectorDomain[String]
class HeaderFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = FeatureDomain
  override def skipNonCategories = true
}

class HeaderTagger(val url:java.net.URL=null) extends DocumentAnnotator {
  class HeaderTaggerCRFModel extends ChainModel[HeaderLabel, HeaderFeatures, Token](
    LabelDomain,
    FeatureDomain,
    l => l.token.attr[HeaderFeatures],
    l => l.token,
    t => t.attr[HeaderLabel]
  )

  val model = new HeaderTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[HeaderLabel].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])//, classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderLabel])

  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.size > 0) {
        val vars = sentence.tokens.map(_.attr[HeaderLabel]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      val vars = document.tokens.map(_.attr[HeaderLabel]).toSeq
      model.maximize(vars)(null)
    }
    document
  }

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[HeaderFeatures]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[HeaderFeatures]
    val tokenSeq = doc.tokens.toSeq
    tokenSeq.foreach(t => {
      t.attr += new HeaderFeatures(t)
      vf(t) ++= TokenFeatures(t)
    })
    LexiconTagger.tagText(tokenSeq, vf)
  }


  def train(trainDocs:Seq[Document], testDocs:Seq[Document], params: HyperParams)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[HeaderLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[HeaderLabel])).toSeq
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", LabelDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", LabelDomain, testLabels.toIndexedSeq))
      }
    }
    val vars = for (td <- trainDocs) yield td.tokens.map(_.attr[HeaderLabel])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    val optimizer = new AdaGradRDA(l1=params.l1, l2=params.l2, delta=params.delta, rate=params.learningRate, numExamples=examples.length)
    println("training...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate, useParallelTrainer=false)
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocs.foreach(process)
    testDocs.foreach(process)
    println("FINAL:")
    val eval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", LabelDomain, testLabels.toIndexedSeq)
    println(eval)
    eval.f1
  }

  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    FeatureDomain.freeze()
    println("Found model")
  }
  else {
    println("model not found")
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(LabelDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(LabelDomain, is)
    LabelDomain.freeze()
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    BinarySerializer.deserialize(model, is)
    is.close()
  }
}

object TrainHeaderTagger extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => trainGrobid(opts)
      case _ => trainDefault(opts)
    }
  }

  def trainDefault(opts: HeaderTaggerOpts): Double = throw new Exception("not yet implemented.")

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    import edu.umass.cs.iesl.paperheader.load.LoadGrobid
    def initGrobidFeatures(docs: Seq[Document]): Unit = {
      docs.flatMap(_.tokens).foreach { token =>
        token.attr += new HeaderFeatures(token)
        token.attr[HeaderFeatures] ++= token.attr[PreFeatures].features
      }
    }
    implicit val random = new scala.util.Random
    val params = new HyperParams(opts)
    val tagger = new HeaderTagger
    val allData = LoadGrobid.fromFilename(opts.trainFile.value, withFeatures=opts.useGrobidFeatures.value)
    println("using labels: " + LabelDomain.categories.mkString(", "))
    val trainPortion = (allData.length.toDouble * opts.trainPortion.value).floor.toInt
    val trainingData = allData.take(trainPortion)
    val testingData = allData.drop(trainPortion)
    println(s"training data: ${trainingData.length} docs, ${trainingData.flatMap(_.tokens).length} tokens")
    println(s"dev data: ${testingData.length} docs, ${testingData.flatMap(_.tokens).length} tokens")
    trainingData.head.tokens.take(5).foreach { t => println(s"${t.string} ${t.attr[HeaderLabel].categoryValue}")}
    // initialize features
    if (opts.useGrobidFeatures.value) {
      initGrobidFeatures(trainingData)
      FeatureDomain.freeze()
      initGrobidFeatures(testingData)
    } else {
      trainingData.foreach(doc => tagger.addFeatures(doc))
      FeatureDomain.freeze()
      testingData.foreach(doc => tagger.addFeatures(doc))
    }
    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    trainingData.head.tokens.take(5).foreach { t => println(s"${t.attr[HeaderFeatures]}")}
    tagger.train(trainingData, testingData, params)
    if (opts.saveModel.value) {
      println(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    val evaluator = new ExactlyLikeGrobidEvaluator
    val (f0, eval) = evaluator.evaluate(testingData, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    println(eval)
    f0
  }
}

object TestHeaderTagger {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => processGrobid(opts)
      case _ => processDefault(opts)
    }
  }
  def processDefault(opts: HeaderTaggerOpts): Unit = throw new Exception("not yet implemented")
  def processGrobid(opts: HeaderTaggerOpts): Unit = {
    val trainer = new HeaderTagger
    trainer.deSerialize(new URL(opts.modelFile.value).openStream())
    println(s"loading file: ${opts.testFile.value} with features? ${opts.useGrobidFeatures.value}")
    val testingData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value)
    if (opts.useGrobidFeatures.value) {
      testingData.flatMap(_.tokens).foreach { token =>
        token.attr += new HeaderFeatures(token)
        token.attr[HeaderFeatures] ++= token.attr[PreFeatures].features
      }
    } else testingData.foreach(doc => trainer.addFeatures(doc))
    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    val tot = trainer.model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = trainer.model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    println(s"model sparsity: $sparsity")
    val labels = testingData.flatMap(_.tokens).map(_.attr[HeaderLabel])
    testingData.foreach(trainer.process)
    val segEval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", LabelDomain, labels.toIndexedSeq)
    println(segEval)
    val evaluator = new ExactlyLikeGrobidEvaluator
    val (_, eval) = evaluator.evaluate(testingData, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    println(eval)
  }
}

case class HyperParams(opts: HeaderTaggerOpts) {
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val learningRate = opts.learningRate.value
  val delta = opts.delta.value
  override def toString(): String = s"HyperParams(l1=$l1 l2=$l2 rate=$learningRate delta=$delta)"
}

class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
  /* data */
  val trainFile = new CmdOption("train-file", "", "STRING", "Filename(s) from which to read training data")
  val trainDir = new CmdOption("train-dir", "", "STRING", "directory of train files")
  val devFile = new CmdOption("dev-file", "", "STRING", "filename of dev set")
  val devDir = new CmdOption("dev-dir", "", "STRING", "directory of dev files")
  val testFile = new CmdOption("test-file", "", "STRING", "Filename(s) from which to read test data")
  val testDir = new CmdOption("test-dir", "", "STRING", "directory of test files")
  val dataSet = new CmdOption("data-set", "", "STRING", "which data set to use (grobid, fullpaper)")

  /* hyperparameters */
  val l1 = new CmdOption("l1", 1e-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 1e-5, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.8515541191715452, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")

  /* serialization */
  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "serialize the model?")
  val modelFile = new CmdOption("model-file", "HeaderTagger.factorie", "STRING", "filename of serialized model")

  /* misc other knobs */
  val rootDir = new CmdOption("root-dir", "", "STRING", "project root")
  val outputDir = new CmdOption("output-dir", "", "STRING", "directory to write evaluations to")
  val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
  val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
}

