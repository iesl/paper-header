package edu.umass.cs.iesl.paperheader.tagger

/**
 * Created by kate on 3/10/15.
 */

import cc.factorie.util._
import cc.factorie.variable._
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._

object ALabels extends CategoricalDomain[String] {
  this ++= Vector("author-firstname", "author-surname", "author-middle"); freeze()
}
object AuthorLabelDomain extends CategoricalDomain[String] with BILOU {
  def baseDomain = ALabels
  this ++= this.bilouTags.toVector
  freeze()
  //TODO spanList
}
class AuthorTag(token: Token, initialCategory: String) extends AbstractHeaderTag(token, initialCategory) { def domain = AuthorLabelDomain }
class LabeledAuthorTag(token: Token, initialCategory: String) extends AuthorTag(token, initialCategory) with CategoricalLabeling[String]

class AuthorNameTagger(val url: java.net.URL=null) extends DocumentAnnotator {
  def tokenAnnotationString(token: Token): String = s"${token.attr[AuthorTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[Sentence], classOf[BilouHeaderTag])
  def postAttrs: Iterable[Class[_]] = List(classOf[AuthorTag])

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token: Token) extends BinaryFeatureVectorVariable[String] { def domain = FeatureDomain; override def skipNonCategories = true }
  class AuthorCRFModel extends ChainModel[AuthorTag, FeatureVariable, Token](
    AuthorLabelDomain,
    FeatureDomain,
    label => label.token.attr[FeatureVariable],
    label => label.token,
    token => token.attr[AuthorTag]
  )
  val model = new AuthorCRFModel
  val objective = cc.factorie.variable.HammingObjective

  def sentenceIsAuthor(sentence: Sentence): Boolean = sentence.tokens.head.attr[BilouHeaderTag].categoryValue.substring(2) == "author"
  def process(document: Document): Document = {
    if (document.tokenCount == 0) return document
    document.tokens.foreach(t => if (!t.attr.contains(classOf[AuthorTag])) t.attr += new AuthorTag(t, "author-firstname"))
    if (!document.hasAnnotation(classOf[FeatureVariable])) for (sentence <- document.sentences if sentence.tokens.size > 0) addFeatures(sentence)
    for (sentence <- document.sentences if sentence.tokens.size > 0) {
      if (sentenceIsAuthor(sentence)) {
        val vars = sentence.tokens.map(_.attr[AuthorTag]).toSeq
        model.maximize(vars)(null)
      }
    }
    document
  }

  def addFeatures(sentence: Sentence): Unit = {
    sentence.document.annotators(classOf[FeatureVariable]) = AuthorNameTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable]
    LexiconTagger.tagText(sentence.tokens.toSeq, vf)
  }

  def train(trainDocs: Seq[Document], testDocs: Seq[Document], delta: Double = 0.1, rate: Double = 0.1)(implicit random: scala.util.Random): Double = {
    def labels(sentences: Seq[Sentence]): IndexedSeq[LabeledAuthorTag] = sentences.flatMap(_.tokens).map(_.attr[LabeledAuthorTag]).toIndexedSeq
    // only take sentences with "author" labels
    val trainSentences = for (td <- trainDocs; sentence <- td.sentences if sentence.tokens.size > 0 && sentenceIsAuthor(sentence)) yield sentence
    val testSentences = for (td <- testDocs; sentence <- td.sentences if sentence.tokens.size > 0 && sentenceIsAuthor(sentence)) yield sentence
    println("adding training features...")
    trainSentences.foreach(addFeatures)
    FeatureDomain.freeze()
    testSentences.foreach(addFeatures)
    val trainLabels = labels(trainSentences); val testLabels = labels(testSentences)
    val vars = for (s <- trainSentences) yield s.tokens.map(_.attr[LabeledAuthorTag])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    println("training...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=new AdaGrad(rate=rate, delta=delta) with ParameterAveraging, useParallelTrainer=false)
    println("Training Evaluation")
    println(new SegmentEvaluation[LabeledAuthorTag]("(B|U)-", "(I|L)-", AuthorLabelDomain, trainLabels))
    println("Testing Evaluation")
    val testEval = new SegmentEvaluation[LabeledAuthorTag]("(B|U)-", "(I|L)-", AuthorLabelDomain, testLabels)
    println(testEval)
    testEval.f1
  }
}

class AuthorTaggerOpts extends DefaultCmdOptions with SharedNLPCmdOptions {
  val headerTaggerModel = new CmdOption("header-tagger-model", "HeaderTagger.factorie", "STRING", "Filename for serialized HeaderTagger model")
  val rate = new CmdOption("rate", 0.1, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")
  val grobidDataDir = new CmdOption("grobid-data-dir", "", "STRING", "directory of grobid data")
}

object AuthorCRFTrainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new AuthorTaggerOpts
    opts.parse(args)
    val (trainDocs, devDocs) = HeaderTaggerUtils.loadDevAndTrainData(opts.grobidDataDir.value, "grobid")
    val headerTagger = new HeaderTagger(url = new java.net.URL("file://" + opts.headerTaggerModel.value))
    println("processing docs with HeaderTagger...")
    trainDocs.foreach(headerTagger.process)
    devDocs.foreach(headerTagger.process)
    val authorTagger = new AuthorNameTagger
    authorTagger.train(trainDocs, devDocs, rate=opts.rate.value, delta=opts.delta.value)
  }
}