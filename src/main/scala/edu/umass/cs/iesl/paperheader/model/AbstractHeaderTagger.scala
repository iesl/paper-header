package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.util.logging.Logger

import cc.factorie.app.chain.{ChainModel, SegmentEvaluation}
import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token}
import cc.factorie.optimize.{AdaGradRDA, L2Regularization, LBFGS, ThreadLocalBatchTrainer, Trainer}
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain, HammingObjective}
import edu.umass.cs.iesl.paperheader.Log

abstract class AbstractHeaderTagger(logFilename: Option[String]) extends DocumentAnnotator with Serializable {

  val log = initLog()
  def initLog(): Logger = {
    logFilename match {
      case Some(fname) => new Log(fname).log
      case None => java.util.logging.Logger.getLogger(getClass.getName)
    }
  }

  val DEFAULT_LABEL: String = "I-other"

  object FeatureDomain extends CategoricalVectorDomain[String]
  class HeaderFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }

  val model = new ChainModel[HeaderLabel, HeaderFeatures, Token](
    HeaderLabelDomain,
    FeatureDomain,
    l => l.token.attr[HeaderFeatures],
    l => l.token,
    t => t.attr[HeaderLabel]
  )

  val objective = HammingObjective

  def sparsity: Double = {
    model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / model.parameters.tensors.sumInts(_.length)
  }

  def process(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[HeaderFeatures])) addFeatures(document)
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.nonEmpty) {
        sentence.tokens.foreach { token => if (!token.attr.contains(classOf[HeaderLabel]))
          token.attr += new HeaderLabel(DEFAULT_LABEL, token) }
        val vars = sentence.tokens.map(_.attr[HeaderLabel]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      document.tokens.foreach { token =>
        if (!token.attr.contains(classOf[HeaderLabel]))
          token.attr += new HeaderLabel(DEFAULT_LABEL, token)
      }
      val vars = document.tokens.map(_.attr[HeaderLabel]).toSeq
      model.maximize(vars)(null)
    }
    document
  }

  def tokenAnnotationString(token:Token): String = s"${token.attr[HeaderLabel].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderLabel])

  def addFeatures(document: Document): Unit

  /**
   * Evaluate this tagger's performance
   * @param labels IndexedSeq[HeaderLabel]
   * @param segmentScheme "BILOU"|"BIO"
   * @return SegmentEvaluation (summary of tagger P/R/F1 on labels)
   */
  def evaluation(labels: IndexedSeq[HeaderLabel], segmentScheme: String): SegmentEvaluation[HeaderLabel] = {
    segmentScheme match {
      case "BILOU" => new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, labels)
      case _ => new SegmentEvaluation[HeaderLabel]("(B|I)-", "I-", HeaderLabelDomain, labels)
    }
  }
  
  
  def train(trainDocuments: Seq[Document],
            devDocuments: Seq[Document],
            params: Hyperparams)(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[HeaderLabel] = docs.flatMap(_.tokens).map(_.attr[HeaderLabel]).toIndexedSeq

    // make sure the label domain contains the default category
    HeaderLabelDomain += DEFAULT_LABEL

    val doTest: Boolean = devDocuments.nonEmpty
    
    val infoStr =
      s"""
         |train set: ${trainDocuments.length} docs with ${trainDocuments.map(_.tokenCount).sum} tokens\n
         |dev set: ${devDocuments.length} docs with ${devDocuments.map(_.tokenCount).sum} tokens
       """.stripMargin
    log.info(infoStr)
    
    if (params.trimBelow > 0) FeatureDomain.dimensionDomain.gatherCounts = true
    trainDocuments.foreach { addFeatures }
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    if (params.trimBelow > 0) {
      FeatureDomain.dimensionDomain.trimBelowCount(params.trimBelow)
      log.info(s"feature domain size (after pruning, cutoff = ${params.trimBelow}): ${FeatureDomain.dimensionSize}")
      FeatureDomain.freeze()
      trainDocuments.foreach { addFeatures }
    } else {
      FeatureDomain.freeze()
    }
    if (doTest) devDocuments.foreach { addFeatures }
    val trainLabels = labels(trainDocuments)
    val testLabels = if (doTest) labels(devDocuments) else IndexedSeq()
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val vars = for (td <- trainDocuments) yield td.tokens.map(_.attr[HeaderLabel])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    var iters = 0
    def evaluate(): Unit = {
      log.info(s"evaluate (iter $iters / ${params.numIterations}})")
      trainDocuments.par.foreach(process)
      log.info(s"train (iter $iters)\n: ${evaluation(trainLabels, params.segmentScheme)}")
      if (doTest) {
        devDocuments.par.foreach(process)
        log.info(s"test (iter $iters)\n: ${evaluation(testLabels, params.segmentScheme)}")
      }
      iters += 1
    }
    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.learningRate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer = false, maxIterations = params.numIterations, optimizer = optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    trainDocuments.par.foreach(process)
    val eval = evaluation(trainLabels, params.segmentScheme)
    log.info(s"train (final):\n$eval")
    if (doTest) {
      devDocuments.par.foreach(process)
      val testEval = evaluation(testLabels, params.segmentScheme)
      log.info(s"test (final):\n$testEval")
      testEval.f1
    } else eval.f1
  }

  def serialize(stream: OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    log.info(s"HeaderLabelDomain size: ${HeaderLabelDomain.size}")
    log.info(s"FeatureDomain size: ${FeatureDomain.dimensionDomain.size}")
    log.info(s"model sparsity: $sparsity")
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderLabelDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    model.serialize(is)
    is.close()
  }

  def deserialize(stream: InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderLabelDomain, is)
    HeaderLabelDomain.freeze()
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    model.deserialize(is)
    is.close()
    log.info(s"HeaderLabelDomain size: ${HeaderLabelDomain.size}")
    log.info(s"FeatureDomain size: ${FeatureDomain.dimensionDomain.size}")
    log.info(s"model sparsity: $sparsity")
  }

}