package edu.umass.cs.iesl.paperheader.model

import java.util.logging.Logger

import cc.factorie._
import cc.factorie.app.chain.{SegmentEvaluation, ChainModel}
import cc.factorie.app.nlp.{DocumentAnnotator, Token, Document}
import cc.factorie.app.nlp.embeddings.SkipGramEmbedding
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.model.DotFamilyWithStatistics2
import cc.factorie.optimize._
import cc.factorie.variable._

import scala.collection.mutable.ListBuffer

/**
 * Created by kate on 11/17/15.
 */
abstract class StackedChainHeaderTagger(rlog: Option[Logger], params: Hyperparams, embeddingMap: SkipGramEmbedding) extends DocumentAnnotator {
  private val log = Logger.getLogger(getClass.getName)
  val resultsLog = rlog match {
    case Some(logger) => logger
    case None => Logger.getLogger(getClass.getName + "-results")
  }

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderTag])
  def tokenAnnotationString(token: Token): String = s"${token.attr[HeaderTag].categoryValue}"

  val embeddingDim = params.embeddingDim
  object EmbeddingDomain extends DiscreteDomain(embeddingDim)
  class EmbeddingVar(t: Tensor1) extends VectorVariable(t) { def domain = EmbeddingDomain }

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVar(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
  }

  object FeatureDomain2 extends CategoricalVectorDomain[String]
  class FeatureVar2(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain2
  }


  class StackedModel(featureDomain1: CategoricalVectorDomain[String],
                     label2features1: HeaderTag => BinaryFeatureVectorVariable[String],
                     label2token1: HeaderTag => Token,
                     token2label1: Token => HeaderTag)
    extends ChainModel(HeaderDomain, featureDomain1, label2features1, label2token1, token2label1) with Parameters {

    val embedding = new DotFamilyWithStatistics2[HeaderTag, EmbeddingVar] {
      val weights = Weights(new DenseTensor2(HeaderDomain.size, embeddingDim))
    }
    val embeddingPrev = new DotFamilyWithStatistics2[HeaderTag, EmbeddingVar] {
      val weights = Weights(new DenseTensor2(HeaderDomain.size, embeddingDim))
    }
    val embeddingNext = new DotFamilyWithStatistics2[HeaderTag, EmbeddingVar] {
      val weights = Weights(new DenseTensor2(HeaderDomain.size, embeddingDim))
    }

    override def factors(vars: Iterable[Var]): Iterable[Factor] = {
      val result = new ListBuffer[Factor]
      vars match {
        case labels: Iterable[HeaderTag] =>
          var prevLabel = null.asInstanceOf[HeaderTag]
          for (label <- labels) {
            result += bias.Factor(label)
            result += obs.Factor(labelToFeatures(label), label)
            if (prevLabel ne null) {
              result += markov.Factor(prevLabel, label)
              if (useObsMarkov) result += obsmarkov.Factor(prevLabel, label, labelToFeatures(label))
            }
            val scale = params.scale
            val useOffsetEmbedding = params.useOffsetEmbedding
            if (embeddingMap != null ) {
              if (embeddingMap.contains(labelToToken(label).string))
                result += embedding.Factor(label, new EmbeddingVar(embeddingMap(labelToToken(label).string) * scale))
              if (useOffsetEmbedding && labelToToken(label).sentenceHasPrev && embeddingMap.contains(labelToToken(label).prev.string))
                result += embeddingPrev.Factor(label, new EmbeddingVar(embeddingMap(labelToToken(label).prev.string) * scale))
              if (useOffsetEmbedding && labelToToken(label).sentenceHasNext && embeddingMap.contains(labelToToken(label).next.string))
                result += embeddingNext.Factor(label, new EmbeddingVar(embeddingMap(labelToToken(label).next.string) * scale))
            }
            prevLabel = label
          }
      }
      result
    }

    override def getLocalScores(varying: Seq[HeaderTag]): Array[DenseTensor1] = {
      val biasScores = bias.weights.value
      val obsWeights = obs.weights.value
      val a = Array.fill[DenseTensor1](varying.size)(null)
      var i = 0
      while (i < varying.length) {
        val scores = obsWeights.leftMultiply(labelToFeatures(varying(i)).value.asInstanceOf[Tensor1]).asInstanceOf[DenseTensor1]
        scores += biasScores
        if (embeddingMap != null) {
          if (embeddingMap.contains(labelToToken(varying(i)).string))
            scores += embedding.weights.value * embeddingMap(labelToToken(varying(i)).string)
          if (i >= 1 && embeddingMap.contains(labelToToken(varying(i-1)).string))
            scores += embeddingPrev.weights.value * embeddingMap(labelToToken(varying(i-1)).string)
          if (i < varying.length-1 && embeddingMap.contains(labelToToken(varying(i+1)).string))
            scores += embeddingNext.weights.value * embeddingMap(labelToToken(varying(i+1)).string)
        }
        a(i) = scores
        i += 1
      }
      a
    }

    override def accumulateExtraObsGradients(gradient: WeightsMapAccumulator, obs: Tensor1, position: Int, labels: Seq[HeaderTag]): Unit = {
      if (embeddingMap ne null) {
        if (embeddingMap.contains(labelToToken(labels(position)).string))
          gradient.accumulate(embedding.weights, obs outer embeddingMap(labelToToken(labels(position)).string))
        if (position >= 1 && embeddingMap.contains(labelToToken(labels(position-1)).string))
          gradient.accumulate(embeddingPrev.weights, obs outer embeddingMap(labelToToken(labels(position-1)).string))
        if (position < labels.length-1 && embeddingMap.contains(labelToToken(labels(position+1)).string))
          gradient.accumulate(embeddingNext.weights, obs outer embeddingMap(labelToToken(labels(position+1)).string))
      }
    }

    def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
  }

  val model = new StackedModel(
    FeatureDomain,
    label => label.token.attr[FeatureVar],
    label => label.token,
    token => token.attr[HeaderTag]
  )

  val model2 = new StackedModel(
    FeatureDomain2,
    label => label.token.attr[FeatureVar2],
    label => label.token,
    token => token.attr[HeaderTag]
  )

  val objective = HammingObjective

  def process(doc: Document): Document = {
    if (doc.tokenCount > 0) {
      val checkToken = doc.tokens.head
      if (!checkToken.attr.contains(classOf[HeaderTag])) {
        doc.tokens.foreach { t => t.attr += new HeaderTag(t, "O")}
      }
      if (!checkToken.attr.contains(classOf[FeatureVar])) {
        doc.tokens.foreach(t => t.attr += new FeatureVar(t))
        addFeatures1(doc, (t: Token) => t.attr[FeatureVar])
      }
      process(doc, useModel2 = false)
      if (!checkToken.attr.contains(classOf[FeatureVar2])) {
        doc.tokens.foreach(t => t.attr += new FeatureVar2(t))
        addFeatures1(doc, (t: Token) => t.attr[FeatureVar2])
        addFeatures2(doc)
      }
      process(doc, useModel2 = true)
    }
    doc
  }

  def process(doc: Document, useModel2: Boolean): Unit = {
    if (doc.tokenCount > 0) {
      val vars = doc.tokens.map(_.attr[HeaderTag]).toSeq
      if (useModel2) model2.maximize(vars)(null)
      else model.maximize(vars)(null)
    }
  }

  def addFeatures1(doc: Document, vf: Token => CategoricalVectorVar[String]): Unit
  def addFeatures2(doc: Document): Unit

  def train(trainDocs: Seq[Document], devDocs: Seq[Document])(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[GoldHeaderTag] = docs.flatMap { doc => doc.tokens.map(t => t.attr[GoldHeaderTag]) }.toIndexedSeq
    resultsLog.info(s"# train docs: ${trainDocs.length} with ${trainDocs.map(_.tokens.size).sum} tokens")
    resultsLog.info(s"# dev docs: ${devDocs.length} with ${devDocs.map(_.tokens.size).sum} tokens")

    /*
    *
    * FIRST MODEL
    *
    */
    log.info(s"adding features for ${trainDocs.length} training documents")
    val vf1 = (t: Token) => t.attr[FeatureVar]
    trainDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar(t))
      addFeatures1(doc, vf1)
    }
    FeatureDomain.freeze()
    log.info(s"feature domain #1 size: ${FeatureDomain.dimensionSize}")
    resultsLog.info(s"feature domain #1 size: ${FeatureDomain.dimensionSize}")
    log.info(s"adding features for ${devDocs.length} dev documents")
    devDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar(t))
      addFeatures1(doc, vf1)
    }

    val trainLabels = labels(trainDocs)
    val devLabels = labels(devDocs)

    def evaluate1(): Unit = {
      trainDocs.foreach(doc => process(doc, useModel2 = false))
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
      devDocs.foreach(doc => process(doc, useModel2 = false))
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
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate1, useParallelTrainer=false, maxIterations=params.iters, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ devLabels).foreach(_.setRandomly)
    evaluate1()


    /*
    *
    * SECOND MODEL
    *
    */
    (trainLabels ++ devLabels).foreach(_.setRandomly)

    val vf2 = (t: Token) => t.attr[FeatureVar2]
    trainDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar2(t))
      addFeatures1(doc, vf2)
      addFeatures2(doc)
    }
    FeatureDomain2.freeze()
    log.info(s"feature domain #2 size: ${FeatureDomain2.dimensionSize}")
    resultsLog.info(s"feature domain #2 size: ${FeatureDomain2.dimensionSize}")
    devDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar2(t))
      addFeatures1(doc, vf2)
      addFeatures2(doc)
    }

    def evaluate2(): Unit = {
      trainDocs.foreach(doc => process(doc, useModel2 = true))
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
      devDocs.foreach(doc => process(doc, useModel2 = true))
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels).toString())
      log.info(s"model #2 sparsity: ${model2.sparsity}")
      log.info(s"objective accuracy: ${objective.accuracy(trainLabels)}")
    }

    val examples2 = {
      val varsByDoc = trainDocs.map(doc => doc.tokens.map(_.attr[GoldHeaderTag]))
      varsByDoc.map { vars => new model.ChainLikelihoodExample(vars.toSeq) }
    }

    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model2.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.learningRate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model2.parameters, examples2, evaluate=evaluate2, useParallelTrainer=false, maxIterations=params.iters, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ devLabels).foreach(_.setRandomly)
    evaluate2()

    resultsLog.info("final (train):")
    resultsLog.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
    val finalEval = new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, devLabels)
    resultsLog.info("final (dev):")
    resultsLog.info(finalEval.toString())
    finalEval.f1
  }

  def train(trainDocs: Seq[Document])(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[GoldHeaderTag] = docs.flatMap { doc => doc.tokens.map(t => t.attr[GoldHeaderTag]) }.toIndexedSeq
    resultsLog.info(s"# train docs: ${trainDocs.length} with ${trainDocs.map(_.tokens.size).sum} tokens")

    /*
    *
    * FIRST MODEL
    *
    */
    log.info(s"adding features for ${trainDocs.length} training documents")
    val vf1 = (t: Token) => t.attr[FeatureVar]
    trainDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar(t))
      addFeatures1(doc, vf1)
    }
    FeatureDomain.freeze()
    log.info(s"feature domain #1 size: ${FeatureDomain.dimensionSize}")
    resultsLog.info(s"feature domain #1 size: ${FeatureDomain.dimensionSize}")

    val trainLabels = labels(trainDocs)

    def evaluate1(): Unit = {
      trainDocs.foreach(doc => process(doc, useModel2 = false))
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
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
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate1, useParallelTrainer=false, maxIterations=params.iters, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    trainLabels.foreach(_.setRandomly)
    evaluate1()

    /*
    *
    * SECOND MODEL
    *
    */
    trainLabels.foreach(_.setRandomly)

    log.info("adding secondary features")
    val vf2 = (t: Token) => t.attr[FeatureVar2]
    trainDocs.foreach { doc =>
      doc.tokens.foreach(t => t.attr += new FeatureVar2(t))
      addFeatures1(doc, vf2)
      addFeatures2(doc)
    }
    FeatureDomain2.freeze()
    log.info(s"feature domain #2 size: ${FeatureDomain2.dimensionSize}")
    resultsLog.info(s"feature domain #2 size: ${FeatureDomain2.dimensionSize}")

    def evaluate2(): Unit = {
      trainDocs.foreach(doc => process(doc, useModel2 = true))
      log.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).toString())
      log.info(s"model #2 sparsity: ${model2.sparsity}")
      log.info(s"objective accuracy: ${objective.accuracy(trainLabels)}")
    }

    val examples2 = {
      val varsByDoc = trainDocs.map(doc => doc.tokens.map(_.attr[GoldHeaderTag]))
      varsByDoc.map { vars => new model.ChainLikelihoodExample(vars.toSeq) }
    }

    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model2.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.learningRate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model2.parameters, examples2, evaluate=evaluate2, useParallelTrainer=false, maxIterations=params.iters, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    trainLabels.foreach(_.setRandomly)
    evaluate2()

    resultsLog.info("final (train):")
    val finalEval = new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels)
    resultsLog.info(finalEval.toString())
    resultsLog.info(s"final model #1 sparsity: ${model.sparsity}")
    resultsLog.info(s"final model #2 sparsity: ${model2.sparsity}")
    finalEval.f1
  }

}
