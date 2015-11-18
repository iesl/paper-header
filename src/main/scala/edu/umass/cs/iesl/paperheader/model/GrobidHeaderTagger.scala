package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.net.URL
import java.time.format.DateTimeFormatter
import java.util.logging.{FileHandler, Logger}
import java.time.LocalDateTime

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.optimize._
import cc.factorie.util.BinarySerializer

/**
 * Created by kate on 11/14/15.
 */

case class GrobidFeatures(features: Array[String], token: Token)

class GrobidHeaderTagger(rlog: Option[Logger]) extends AbstractHeaderTagger(rlog) {

  private val log = Logger.getLogger(getClass.getName)
  
  override def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[GrobidFeatures])

  def this(resultsLog: Option[Logger], url: URL) = {
    this(resultsLog)
    deserialize(url.openConnection.getInputStream)
    log.info(s"loaded model from ${url.getPath}")
  }

  def this(resultsLog: Option[Logger], modelPath: String) = {
    this(resultsLog, new URL("file://" + modelPath))
  }

  def process(doc: Document): Document = {
    if (doc.tokenCount > 0) {
      if (!doc.tokens.head.attr.contains(classOf[FeatureVar])) addFeatures(doc)
      if (!doc.tokens.head.attr.contains(classOf[HeaderTag])) {
        doc.tokens.foreach { token => token.attr += new HeaderTag(token, "I-abstract") }
      }
      val vars = doc.tokens.map(token => token.attr[HeaderTag]).toSeq
      model.maximize(vars)(null)
    }
    doc
  }

  def addFeatures(doc: Document): Unit = {
    doc.tokens.foreach { token =>
      val fv = new FeatureVar(token)
      val grobidFeatures = token.attr[GrobidFeatures].features
      grobidFeatures.zipWithIndex.foreach { case (fval, idx) => fv += s"G@$idx=$fval" }
      token.attr += fv
    }
  }

  def train(trainDocs: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[GoldHeaderTag] = docs.flatMap { doc => doc.tokens.map(t => t.attr[GoldHeaderTag]) }.toIndexedSeq
    resultsLog.info(s"# train docs: ${trainDocs.length}, # tokens: ${trainDocs.map(_.tokens.size).sum}")
    resultsLog.info(s"HeaderDomain size: ${HeaderDomain.size}")
    log.info(s"adding features for ${trainDocs.length} training documents")
    trainDocs.foreach(addFeatures)
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    resultsLog.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    FeatureDomain.freeze()
    val trainLabels = labels(trainDocs)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      val eval = new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels)
      log.info(eval.toString())
      log.info(s"model sparsity: ${model.sparsity}")
      log.info(s"objective accuracy: ${objective.accuracy(trainLabels)}")
    }
    val examples = {
      val varsByDoc = trainDocs.map(doc => doc.tokens.map(_.attr[GoldHeaderTag]))
      varsByDoc.map { vars => new model.ChainLikelihoodExample(vars.toSeq) }
    }
    log.info(s"training using ${examples.length} examples")
    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.learningRate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.iters, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    trainLabels.foreach(_.setRandomly)
    evaluate()
    val finalEval = new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels)
    resultsLog.info("final train evaluation:")
    resultsLog.info(finalEval.toString())
    resultsLog.info(s"final objective accuracy: ${objective.accuracy(trainLabels)}")
    resultsLog.info(s"final model sparsity: ${model.sparsity}")
    finalEval.f1
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderDomain, is)
    log.info(s"serialized label domain: label domain size = ${HeaderDomain.size}")
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    log.info(s"serialized feature domain: feature domain size = ${FeatureDomain.dimensionDomain.size}")
    BinarySerializer.serialize(model, is)
    log.info(s"serialized model: model sparsity = ${model.sparsity}")
    is.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderDomain, is)
    HeaderDomain.freeze()
    log.info(s"label domain size: ${HeaderDomain.size}")
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    log.info(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    BinarySerializer.deserialize(model, is)
    log.info(s"model sparsity: ${model.sparsity}")
    is.close()
  }

}
