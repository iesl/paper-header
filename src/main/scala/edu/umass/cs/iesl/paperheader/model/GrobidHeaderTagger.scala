package edu.umass.cs.iesl.paperheader.model

import java.util.logging.Logger

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.optimize._

/**
 * Created by kate on 11/14/15.
 */

case class GrobidFeatures(features: Array[String], token: Token)

class GrobidHeaderTagger extends AbstractHeaderTagger {

  private val log = Logger.getLogger(getClass.getName)

  override def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[GrobidFeatures])

  def process(doc: Document): Document = {
    if (doc.tokenCount > 0) {
      if (!doc.tokens.head.attr.contains(classOf[FeatureVar])) addFeatures(doc)
      if (!doc.tokens.head.attr.contains(classOf[HeaderTag])) {
        doc.tokens.foreach { token => token.attr += new HeaderTag(token, "O") }
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
    log.info(s"adding features for ${trainDocs.length} training documents")
    trainDocs.foreach(addFeatures)
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    FeatureDomain.freeze()
    val trainLabels = labels(trainDocs)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels))
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
    new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels).f1
  }

}
