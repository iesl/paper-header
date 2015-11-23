package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.net.URL

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.lemma.{LowercaseTokenLemma, LowercaseLemmatizer}
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.lexicon.StaticLexicons

import java.util.logging.Logger

import cc.factorie.optimize._
import cc.factorie.util.BinarySerializer

/**
 * Created by kate on 11/14/15.
 */
class CombinedHeaderTagger(rlog: Option[Logger], lexicon: StaticLexicons) extends AbstractHeaderTagger(rlog) {

  private val log = Logger.getLogger(getClass.getName)

  override def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[GrobidFeatures])

  def this(rlog: Option[Logger], lexicon: StaticLexicons, url: URL) = {
    this(rlog, lexicon)
    deserialize(url.openConnection.getInputStream)
    log.info(s"loaded model from ${url.getPath}")
  }

  def this(rlog: Option[Logger], lexicon: StaticLexicons, modelPath: String) = {
    this(rlog, lexicon, new URL("file://" + modelPath))
  }

  lexicon.synchronized {
    lexicon.iesl.Month.toString()
    lexicon.iesl.Day.toString()

    lexicon.iesl.PersonFirst.toString()
    lexicon.iesl.PersonFirstHigh.toString()
    lexicon.iesl.PersonFirstHighest.toString()
    lexicon.iesl.PersonFirstMedium.toString()

    lexicon.iesl.PersonLast.toString()
    lexicon.iesl.PersonLastHigh.toString()
    lexicon.iesl.PersonLastHighest.toString()
    lexicon.iesl.PersonLastMedium.toString()

    lexicon.iesl.PersonHonorific.toString()

    lexicon.iesl.Company.toString()
    lexicon.iesl.JobTitle.toString()
    lexicon.iesl.OrgSuffix.toString()

    lexicon.iesl.Country.toString()
    lexicon.iesl.City.toString()
    lexicon.iesl.PlaceSuffix.toString()
    lexicon.iesl.UsState.toString()
    lexicon.iesl.Continents.toString()

    lexicon.wikipedia.Person.toString()
    lexicon.wikipedia.Event.toString()
    lexicon.wikipedia.Location.toString()
    lexicon.wikipedia.Organization.toString()
    lexicon.wikipedia.ManMadeThing.toString()
    lexicon.iesl.Demonym.toString()

    lexicon.wikipedia.Book.toString()
    lexicon.wikipedia.Business.toString()
    lexicon.wikipedia.Film.toString()

    lexicon.wikipedia.LocationAndRedirect.toString()
    lexicon.wikipedia.PersonAndRedirect.toString()
    lexicon.wikipedia.OrganizationAndRedirect.toString()
    log.info("loaded lexicons")
  }

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

  val FEATURE_PREFIX_REGEX = "^[^@]*$".r

  def addFeatures(doc: Document): Unit = {
    LowercaseLemmatizer.process(doc)
    val lemmaFxn = (t: Token) => t.attr[LowercaseTokenLemma].value
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach { token =>
      val fv = new FeatureVar(token)
      val grobidFeatures = token.attr[GrobidFeatures].features
      grobidFeatures.zipWithIndex.foreach { case (fval, idx) => fv += s"G_$idx=$fval" }
      fv ++= FeatureExtractor.firstOrderFeatures(token)
      token.attr += fv
    }
    val vf = (t: Token) => t.attr[FeatureVar]
    lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH", lemmaFxn)
    lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY", lemmaFxn)

    lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST", lemmaFxn)
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH", lemmaFxn)
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST", lemmaFxn)
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM", lemmaFxn)

    lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST", lemmaFxn)
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH", lemmaFxn)
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST", lemmaFxn)
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM", lemmaFxn)

    lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC", lemmaFxn)

    lexicon.iesl.Company.tagText(tokenSequence,vf, "COMPANY", lemmaFxn)
    lexicon.iesl.JobTitle.tagText(tokenSequence,vf, "JOB-TITLE", lemmaFxn)
    lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf, "ORG-SUFFIX", lemmaFxn)

    lexicon.iesl.Country.tagText(tokenSequence,vf, "COUNTRY", lemmaFxn)
    lexicon.iesl.City.tagText(tokenSequence,vf, "CITY", lemmaFxn)
    lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf, "PLACE-SUFFIX", lemmaFxn)
    lexicon.iesl.UsState.tagText(tokenSequence,vf, "USSTATE", lemmaFxn)
    lexicon.iesl.Continents.tagText(tokenSequence,vf, "CONTINENT", lemmaFxn)

    lexicon.wikipedia.Person.tagText(tokenSequence,vf, "WIKI-PERSON", lemmaFxn)
    lexicon.wikipedia.Event.tagText(tokenSequence,vf, "WIKI-EVENT", lemmaFxn)
    lexicon.wikipedia.Location.tagText(tokenSequence,vf, "WIKI-LOCATION", lemmaFxn)
    lexicon.wikipedia.Organization.tagText(tokenSequence,vf, "WIKI-ORG", lemmaFxn)
    lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf, "MANMADE", lemmaFxn)
    lexicon.iesl.Demonym.tagText(tokenSequence,vf, "DEMONYM", lemmaFxn)

    lexicon.wikipedia.Book.tagText(tokenSequence,vf, "WIKI-BOOK", lemmaFxn)
    lexicon.wikipedia.Business.tagText(tokenSequence,vf, "WIKI-BUSINESS", lemmaFxn)
    lexicon.wikipedia.Film.tagText(tokenSequence,vf, "WIKI-FILM", lemmaFxn)

    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf, "WIKI-LOCATION-REDIRECT", lemmaFxn)
    lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf, "WIKI-PERSON-REDIRECT", lemmaFxn)
    lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf, "WIKI-ORG-REDIRECT", lemmaFxn)

    cc.factorie.app.chain.Observations.addNeighboringFeatures(tokenSequence.toIndexedSeq, vf, FEATURE_PREFIX_REGEX, -2, 2)

  }

  def train(trainDocs: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    def labels(docs: Seq[Document]): IndexedSeq[GoldHeaderTag] = docs.flatMap { doc => doc.tokens.map(t => t.attr[GoldHeaderTag]) }.toIndexedSeq
    resultsLog.info(s"# train docs: ${trainDocs.length}, # tokens: ${trainDocs.map(_.tokens.size).sum}")
    log.info(s"adding features for ${trainDocs.length} training documents")
    trainDocs.foreach(addFeatures)
    log.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    resultsLog.info(s"feature domain size: ${FeatureDomain.dimensionSize}")
    FeatureDomain.freeze()
    val trainLabels = labels(trainDocs)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, trainLabels))
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
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
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
