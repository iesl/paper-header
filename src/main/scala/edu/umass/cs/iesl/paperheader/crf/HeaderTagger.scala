package edu.umass.cs.iesl.paperheader.crf

import java.io.{BufferedInputStream, BufferedOutputStream, File}

import cc.factorie._
import cc.factorie.optimize.{Trainer, AdaGrad}
import cc.factorie.app.nlp._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable._
import cc.factorie.app.chain._
import java.io._

/**
 * Created by kate on 9/25/14.
 */

/**
 * DEV PERFORMANCE
 * Final Testing: accuracy=0.8049933125278644
OVERALL  f1=0.409972 p=0.432749 r=0.389474 (tp=148 fp=194 fn=232 true=380 pred=342) acc=0.804993 (9028/11215)
abstract f1=0.688172 p=0.627451 r=0.761905 (tp=32 fp=19 fn=10 true=42 pred=51)
address  f1=0.463415 p=0.441860 r=0.487179 (tp=19 fp=24 fn=20 true=39 pred=43)
author   f1=0.226804 p=0.309859 r=0.178862 (tp=22 fp=49 fn=101 true=123 pred=71)
date     f1=0.600000 p=0.750000 r=0.500000 (tp=6 fp=2 fn=6 true=12 pred=8)
email    f1=0.545455 p=0.571429 r=0.521739 (tp=12 fp=9 fn=11 true=23 pred=21)
institution f1=0.378947 p=0.346154 r=0.418605 (tp=18 fp=34 fn=25 true=43 pred=52)
keyword  f1=0.285714 p=0.285714 r=0.285714 (tp=2 fp=5 fn=5 true=7 pred=7)
note     f1=0.168675 p=0.170732 r=0.166667 (tp=7 fp=34 fn=35 true=42 pred=41)
tech     f1=0.000000 p=0.000000 r=0.000000 (tp=0 fp=1 fn=3 true=3 pred=1)
thesis   f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
title    f1=0.645161 p=0.638298 r=0.652174 (tp=30 fp=17 fn=16 true=46 pred=47)

 * @param url
 */

class HeaderTagger(val url:java.net.URL=null) extends DocumentAnnotator {
  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    FeatureDomain.freeze()
    println("Found model")
  } else {
    println("model not found")
  }

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }
  class HeaderTaggerModel extends ChainModel[BioHeaderTag, FeatureVariable, Token](
    BioHeaderTagDomain,
    FeatureDomain,
    label => label.token.attr[FeatureVariable],
    label => label.token,
    token => token.attr[BioHeaderTag]
  )
  val model = new HeaderTaggerModel

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BioHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[BioHeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val tokens = document.sections.flatMap(_.tokens)
    tokens.foreach(token => if (!token.attr.contains(classOf[BioHeaderTag])) token.attr += new BioHeaderTag(token, "I-abstract"))
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    // TODO for now, each document is an example
    val labels = tokens.map(_.attr[BioHeaderTag])
    model.maximize(labels)(null)
    if (!alreadyHadFeatures) {
      document.annotators.remove(classOf[FeatureVariable])
      tokens.foreach(token => token.attr.remove[FeatureVariable])
    }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => BioHeaderTagDomain.spanList(section)))
    document
  }

  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    val tokenSeq = doc.sections.flatMap(_.tokens).toSeq
    val vf = (t:Token) => t.attr[FeatureVariable]
    //token-local features
    tokenSeq.foreach(token => {
      token.attr += new FeatureVariable(token)
      vf(token) ++= Features.extractTokenFeatures(token)
    })
    Features.tagWithLexicons(tokenSeq, vf)
    addNeighboringFeatureConjunctions(tokenSeq.toIndexedSeq, vf, List(0), List(1), List(-1), List(2), List(-2))
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], lr:Double=0.1, delta:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBioHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBioHeaderTag]))
    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.par.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    // TODO for now, each document is an example
    val examples = for (doc <- trainDocs) yield new model.ChainLikelihoodExample(doc.sections.flatMap(_.tokens).map(_.attr[LabeledBioHeaderTag]))
    val optimizer = new AdaGrad(rate=lr, delta=delta)
    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      testDocs.par.foreach(process)
      println(s"Training: accuracy=${HammingObjective.accuracy(trainLabels)}")
      println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, trainLabels.toIndexedSeq))
      println(s"Testing: accuracy=${HammingObjective.accuracy(testLabels)}")
      println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq))
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    println("training...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    trainDocs.par.foreach(process)
    testDocs.par.foreach(process)
    println(s"Final Training: accuracy=${HammingObjective.accuracy(trainLabels)}")
    println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, trainLabels.toIndexedSeq))
    println(s"Final Testing: accuracy=${HammingObjective.accuracy(testLabels)}")
    println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq))
    new app.chain.SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq).f1
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    is.close()
  }
}

//object HeaderTaggerCRF extends HeaderTagger(url=new java.net.URL("jar:file://crf/HeaderTagger.factorie"))

class HeaderTaggerOpts extends cc.factorie.util.CmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "HeaderTagger.factorie", "STRING", "Filename for the model (saving a trained model or reading a running model.")
  val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize at all")
  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data")
  val dev = new CmdOption("dev", "", "STRING", "Filename from which to read development data")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data")
  val grobidData = new CmdOption("grobid-data", "", "STRING", "filename for grobid data")
  val l1 = new CmdOption("l1", 1.424388380418031E-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 0.06765909781125444, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.13090758155861615, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val delta = new CmdOption("delta", 0.0010256734204144803, "DOUBLE", "delta for AdaGrad")
  //--learning-rate=0.13090758155861615 --delta=0.0010256734204144803
}

object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger
    var trainDocs = LoadTSV(opts.train.value)
    val devDocs = LoadTSV(opts.dev.value)
    if (opts.grobidData.wasInvoked) {
      val grobid = LoadGrobid.fromDirectory(opts.grobidData.value)
      trainDocs ++= grobid
    }
    println(s"using ${trainDocs.length} train docs; ${devDocs.length} dev docs")
    println(s"using hyperparams: lr=${opts.learningRate.value}, delta=${opts.delta.value}")
    val result = tagger.train(trainDocs, devDocs, lr=opts.learningRate.value, delta=opts.delta.value)
    if (opts.serialize.value){
      println(s"serializing model to: ${opts.saveModel.value}")
      tagger.serialize(new FileOutputStream(opts.saveModel.value))
    }
    result
  }
}

object HeaderTaggerOptimizer {
  def main(args: Array[String]): Unit = {
    import cc.factorie.util.{HyperParameter, LogUniformDoubleSampler, HyperParameterSearcher}
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.serialize.setValue(false)
//    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-8, 1))
//    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-8, 1))
    val lr = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-6, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-6, 1))
    val qs = new cc.factorie.util.QSubExecutor(10, "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(lr, delta), qs.execute, 100, 90, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.serialize.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}

object HeaderTaggerTester {
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger//(url=new java.net.URL("file://" + opts.saveModel.value))
//    val modelURL = new java.net.URL("file://" + opts.saveModel.value)
    tagger.deSerialize(new FileInputStream(opts.saveModel.value))
    val docs = LoadTSV(opts.test.value)
    docs.foreach(tagger.process)
  }
}



