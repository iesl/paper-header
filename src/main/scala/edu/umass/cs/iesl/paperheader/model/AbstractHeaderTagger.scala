package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.util.logging.Logger

import cc.factorie._
import cc.factorie.app.chain.{ChainModel, SegmentEvaluation}
import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token}
import cc.factorie.optimize._
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain, HammingObjective}

/**
 * Created by kate on 11/14/15.
 */
abstract class AbstractHeaderTagger(rlog: Option[Logger]) extends DocumentAnnotator {

  private val log = Logger.getLogger(getClass.getName)
  val resultsLog = rlog match {
    case Some(logger) => logger
    case None => Logger.getLogger(getClass.getName + "-results")
  }

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderTag])
  def tokenAnnotationString(token: Token): String = s"${token.attr[HeaderTag].categoryValue}"

  def process(doc: Document): Document

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVar(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
  }
  class Model extends ChainModel[HeaderTag, FeatureVar, Token](
    HeaderDomain,
    FeatureDomain,
    tag => tag.token.attr[FeatureVar],
    tag => tag.token,
    token => token.attr[HeaderTag]
  ) {
    def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
  }

  lazy val model = new Model
  val objective = HammingObjective

  def addFeatures(doc: Document): Unit

  def train(trainDocs: Seq[Document], devDocs: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[GoldHeaderTag] = docs.flatMap { doc => doc.tokens.map(t => t.attr[GoldHeaderTag]) }.toIndexedSeq
    resultsLog.info(s"# train docs: ${trainDocs.length} with ${trainDocs.map(_.tokens.size).sum} tokens")
    resultsLog.info(s"# dev docs: ${devDocs.length} with ${devDocs.map(_.tokens.size).sum} tokens")
    log.info(s"adding features for ${trainDocs.length} training documents")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    resultsLog.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    log.info(s"adding features for ${devDocs.length} dev documents")
    devDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val devLabels = labels(devDocs)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
      devDocs.foreach(process)
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels).toString())
      log.info(s"model sparsity: ${model.sparsity}")
      log.info(s"objective accuracy: ${objective.accuracy(trainLabels)}")
    }
    val examples = {
      val varsByDoc = trainDocs.map(doc => doc.tokens.map(_.attr[GoldHeaderTag]))
      varsByDoc.map { vars => new model.ChainLikelihoodExample(vars.toSeq) }
    }
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
    (trainLabels ++ devLabels).foreach(_.setRandomly)
    evaluate()
    resultsLog.info("final (train):")
    resultsLog.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
    val finalEval = new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels)
    resultsLog.info("final (dev):")
    resultsLog.info(finalEval.toString())
    finalEval.f1
  }

  def serialize(stream: OutputStream): Unit
  def deserialize(stream: InputStream): Unit



}
