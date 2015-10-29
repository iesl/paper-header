package edu.umass.cs.iesl.paperheader.tagger

import java.net.URL

import edu.umass.cs.iesl.paperheader.load.LoadIESL


import java.io._
import java.util.logging.Logger

import cc.factorie.app.chain.{SegmentEvaluation, ChainModel}
import cc.factorie.app.nlp.{Document, Token, DocumentAnnotator}
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.optimize.{Trainer, AdaGradRDA}
import cc.factorie.util.{HyperparameterMain, BinarySerializer}
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain}

/**
 * Created by kate on 10/28/15.
 */
class BIOHeaderTagger(lexicon: StaticLexicons) extends DocumentAnnotator {
  private val log = Logger.getLogger(getClass.getName)

  /* Deserialize this HeaderTagger from the model at the given URL */
  def this(lexicon: StaticLexicons, url:java.net.URL) = {
    this(lexicon)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      println("Found model")
    }
    else {
      println("model not found")
    }
  }

  /* Deserialize this HeaderTagger from the model at the given path on disk */
  def this(lexicon: StaticLexicons, modelPath: String) = {
    this(lexicon, new URL("file://" + modelPath))
  }
  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BIOHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BIOHeaderTag])

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


  object MyFeatureDomain extends CategoricalVectorDomain[String]
  class MyFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = MyFeatureDomain
    override def skipNonCategories = true
  }

  class HeaderTaggerCRFModel extends ChainModel[BIOHeaderTag, MyFeatures, Token](
    BIOHeaderDomain,
    MyFeatureDomain,
    l => l.token.attr[MyFeatures],
    l => l.token,
    t => t.attr[BIOHeaderTag]
  ){
    def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
  }

  val model = new HeaderTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective

  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[MyFeatures])) {
//      println("initializing features...")
      addFeatures(document)
    }
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.size > 0) {
        sentence.tokens.foreach { token => if (!token.attr.contains(classOf[BIOHeaderTag])) token.attr += new BIOHeaderTag(token, "O") }
        val vars = sentence.tokens.map(_.attr[BIOHeaderTag]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      document.tokens.foreach { token => if (!token.attr.contains(classOf[BIOHeaderTag])) token.attr += new BIOHeaderTag(token, "O") }
      val vars = document.tokens.map(_.attr[BIOHeaderTag]).toSeq
      model.maximize(vars)(null)
    }
    document
  }

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[MyFeatures]) = BIOHeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[MyFeatures]
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach(t => {
      t.attr += new MyFeatures(t)
      vf(t) ++= TokenFeatures(t)
    })
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
    //    lexiconTagger.tagText(tokenSeq, vf)
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], params: HyperParams)(implicit random: scala.util.Random): Double = {
    // init features
    println("computing features")
    trainDocs.foreach(addFeatures)
    MyFeatureDomain.freeze()

    println(s"feature domain size: ${MyFeatureDomain.dimensionDomain.size}")
    trainDocs.head.tokens.take(5).foreach { t => println(s"${t.attr[MyFeatures]}")}

    testDocs.foreach(addFeatures)

    def labels(docs:Seq[Document]): Seq[LabeledBIOHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBIOHeaderTag]))
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[LabeledBIOHeaderTag]("(B|I)-", "I-", BIOHeaderDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBIOHeaderTag]("(B|I)-", "I-", BIOHeaderDomain, testLabels.toIndexedSeq))
      }
    }
    val vars = for (td <- trainDocs) yield td.tokens.map(_.attr[LabeledBIOHeaderTag])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    val optimizer = new AdaGradRDA(l1=params.l1, l2=params.l2, delta=params.delta, rate=params.learningRate, numExamples=examples.length)
    println("training...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, maxIterations=params.iters, evaluate=evaluate, useParallelTrainer=false)
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocs.foreach(process)
    testDocs.foreach(process)
    println("FINAL (train):")
    val trainEval = new SegmentEvaluation[LabeledBIOHeaderTag]("(B|I)-", "I-", BIOHeaderDomain, trainLabels.toIndexedSeq)
    println(trainEval)

    println("FINAL (test):")
    val testEval = new SegmentEvaluation[LabeledBIOHeaderTag]("(B|I)-", "I-", BIOHeaderDomain, testLabels.toIndexedSeq)
    println(testEval)

    params.eval match {
      case "train" => trainEval.f1
      case _ => testEval.f1
    }
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(BIOHeaderDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderLabelDomain, is)
    HeaderLabelDomain.freeze()
    println("BIO Header Domain: " + BIOHeaderDomain.categories.mkString(", "))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    BinarySerializer.deserialize(model, is)
    println(s"model sparsity: ${model.sparsity}")
    is.close()
  }
  
}

object TrainBIOHeaderTagger extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new BIOHeaderTagger(lexicon)

    val trainDocs = LoadIESL.fromFilename(opts.trainFile.value)
    val devDocs = LoadIESL.fromFilename(opts.devFile.value)

    val params = new HyperParams(opts)

    val f1 = tagger.train(trainDocs, devDocs, params)

    if (opts.saveModel.value) {
      println(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }

    f1
  }
}
