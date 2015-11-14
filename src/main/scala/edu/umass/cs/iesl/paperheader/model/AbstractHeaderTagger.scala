package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.util.logging.Logger

import cc.factorie._
import cc.factorie.app.chain.{ChainModel, SegmentEvaluation}
import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token}
import cc.factorie.optimize._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalDomain, HammingObjective}

/**
 * Created by kate on 11/14/15.
 */
abstract class AbstractHeaderTagger extends DocumentAnnotator {

  private val log = Logger.getLogger(getClass.getName)

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderTag])
  def tokenAnnotationString(token: Token): String = s"${token.attr[HeaderTag].categoryValue}"

  def process(doc: Document): Document

  object FeatureDomain extends CategoricalDomain[String]
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
    log.info(s"adding features for ${trainDocs.length} training documents")
    trainDocs.foreach(addFeatures)
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    FeatureDomain.freeze()
    log.info(s"adding features for ${devDocs.length} dev documents")
    devDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val devLabels = labels(devDocs)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels))
      devDocs.foreach(process)
      println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels))
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
    new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels).f1
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderDomain, is)
    HeaderDomain.freeze()
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    BinarySerializer.deserialize(model, is)
    println(s"model sparsity: ${model.sparsity}")
    is.close()
  }

}
