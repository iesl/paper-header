package edu.umass.cs.iesl.paperheader.model

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.lexicon.StaticLexicons

import java.util.logging.Logger

import cc.factorie.optimize._

/**
 * Created by kate on 11/14/15.
 */
class CombinedHeaderTagger(lexicon: StaticLexicons) extends AbstractHeaderTagger {

  private val log = Logger.getLogger(getClass.getName)

  override def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[GrobidFeatures])

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

  def addFeatures(doc: Document): Unit = {
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach { token =>
      val fv = new FeatureVar(token)
      val grobidFeatures = token.attr[GrobidFeatures].features
      grobidFeatures.zipWithIndex.foreach { case (fval, idx) => fv += s"G@$idx=$fval" }
      fv ++= FeatureExtractor.process(token)
      token.attr += fv
    }
    val vf = (t: Token) => t.attr[FeatureVar]
    lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH")
    lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY")

    lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM")

    lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM")

    lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC")

    lexicon.iesl.Company.tagText(tokenSequence,vf, "COMPANY")
    lexicon.iesl.JobTitle.tagText(tokenSequence,vf, "JOB-TITLE")
    lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf, "ORG-SUFFIX")

    lexicon.iesl.Country.tagText(tokenSequence,vf, "COUNTRY")
    lexicon.iesl.City.tagText(tokenSequence,vf, "CITY")
    lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf, "PLACE-SUFFIX")
    lexicon.iesl.UsState.tagText(tokenSequence,vf, "USSTATE")
    lexicon.iesl.Continents.tagText(tokenSequence,vf, "CONTINENT")

    lexicon.wikipedia.Person.tagText(tokenSequence,vf, "WIKI-PERSON")
    lexicon.wikipedia.Event.tagText(tokenSequence,vf, "WIKI-EVENT")
    lexicon.wikipedia.Location.tagText(tokenSequence,vf, "WIKI-LOCATION")
    lexicon.wikipedia.Organization.tagText(tokenSequence,vf, "WIKI-ORG")
    lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf, "MANMADE")
    lexicon.iesl.Demonym.tagText(tokenSequence,vf, "DEMONYM")

    lexicon.wikipedia.Book.tagText(tokenSequence,vf, "WIKI-BOOK")
    lexicon.wikipedia.Business.tagText(tokenSequence,vf, "WIKI-BUSINESS")
    lexicon.wikipedia.Film.tagText(tokenSequence,vf, "WIKI-FILM")

    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf, "WIKI-LOCATION-REDIRECT")
    lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf, "WIKI-PERSON-REDIRECT")
    lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf, "WIKI-ORG-REDIRECT")
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
