package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie._
import cc.factorie.app.nlp.lexicon.{StaticLexicons,LexiconsProvider}
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.util._
import cc.factorie.variable._
import cc.factorie.app.chain.{ChainModel, SegmentEvaluation}
import edu.umass.cs.iesl.paperheader.load._
import java.io._
import java.net.URL
import java.util.logging.Logger


/**
 * Created by kate on 9/25/14.
 *
 * TODO load Brown clusters
 */

object HeaderLabelDomain extends CategoricalDomain[String]

abstract class HLabel(labelname: String) extends LabeledCategoricalVariable(labelname)
class HeaderLabel(labelname: String, val token: Token) extends HLabel(labelname) {
  def domain = HeaderLabelDomain
}

object FeatureDomain extends CategoricalVectorDomain[String]
class HeaderFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = FeatureDomain
  override def skipNonCategories = true
}

class HeaderTagger(lexicon: StaticLexicons) extends DocumentAnnotator {

  private val log = Logger.getLogger(getClass.getName)

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[HeaderLabel].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderLabel])

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

  class HeaderTaggerCRFModel extends ChainModel[HeaderLabel, HeaderFeatures, Token](
    HeaderLabelDomain,
    FeatureDomain,
    l => l.token.attr[HeaderFeatures],
    l => l.token,
    t => t.attr[HeaderLabel]
  ){
    def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
  }

  val model = new HeaderTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective



  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[HeaderFeatures])) {
      //      println("initializing features...")
      addFeatures(document)
    }
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.size > 0) {
        sentence.tokens.foreach { token => if (!token.attr.contains(classOf[HeaderLabel])) token.attr += new HeaderLabel("O", token) }
        val vars = sentence.tokens.map(_.attr[HeaderLabel]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      document.tokens.foreach { token => if (!token.attr.contains(classOf[HeaderLabel])) token.attr += new HeaderLabel("O", token) }
      val vars = document.tokens.map(_.attr[HeaderLabel]).toSeq
      model.maximize(vars)(null)
    }
    postProcess(document)
    document
  }

  def postProcess(document: Document, bilou: Boolean = false) = {
    var lastLabel: HeaderLabel = null
    var lastLabelBilou = ' '
    var lastLabelRest = ""
    document.tokens.foreach { token =>
      val label = token.attr[HeaderLabel]
      val labelString = label.categoryValue
      val labelBilou = labelString(0)
      val labelRest = labelString.drop(2)
      if (bilou) {
        if (lastLabel == null) {
          label.set(HeaderLabelDomain.index("B-" + labelRest))(null)
        }
        else if (labelRest != lastLabelRest) {
          if (lastLabelBilou == 'B') lastLabel.set(HeaderLabelDomain.index("U-" + lastLabelRest))(null)
          else lastLabel.set(HeaderLabelDomain.index("L-" + lastLabelRest))(null)
          label.set(HeaderLabelDomain.index("B-" + labelRest))(null)
        }
        else label.set(HeaderLabelDomain.index("I-" + labelRest))(null)
      }
      else{
        if (lastLabel == null) label.set(HeaderLabelDomain.index("B-" + labelRest))(null)
        else if (labelRest != lastLabelRest) label.set(HeaderLabelDomain.index("B-" + labelRest))(null)
      }
      lastLabel = label
      lastLabelBilou = labelBilou
      lastLabelRest = labelRest
    }
    if(bilou) {
      if (lastLabelBilou == 'B') lastLabel.set(HeaderLabelDomain.index("U-" + lastLabelRest))(null)
      else lastLabel.set(HeaderLabelDomain.index("L-" + lastLabelRest))(null)
    }
  }

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[HeaderFeatures]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[HeaderFeatures]
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach(t => {
      t.attr += new HeaderFeatures(t)
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
    cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(doc.tokens.toIndexedSeq, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))

    //    lexiconTagger.tagText(tokenSeq, vf)
  }


  def train(trainDocs:Seq[Document], testDocs:Seq[Document], params: HyperParams)(implicit random: scala.util.Random): Double = {
    // init features
    println("computing features")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()

    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    trainDocs.head.tokens.take(5).foreach { t => println(s"${t.attr[HeaderFeatures]}")}

    testDocs.foreach(addFeatures)

    def labels(docs:Seq[Document]): Seq[HeaderLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[HeaderLabel]))
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    var iters = 0
    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      println(s"(iter $iters) Train accuracy (overall): "+objective.accuracy(trainLabels))
      println(s"(iter $iters) Training:")
      println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println(s"(iter $iters) Dev  accuracy (overall): "+objective.accuracy(testLabels))
        println(s"(iter $iters) Dev:")
        println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, testLabels.toIndexedSeq))
      }
      iters += 1
    }
    val vars = for (td <- trainDocs) yield td.tokens.map(_.attr[HeaderLabel])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    val optimizer = new AdaGrad(delta=params.delta, rate=params.learningRate) with ParameterAveraging
    println(s"training using ${examples.length} examples ...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, maxIterations=params.iters, evaluate=evaluate, useParallelTrainer=false)

    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocs.foreach(process)
    testDocs.foreach(process)
    println("FINAL (train):")
    val trainEval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, trainLabels.toIndexedSeq)
    println(trainEval)

    println("FINAL (dev):")
    val testEval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, testLabels.toIndexedSeq)
    println(testEval)

    params.eval match {
      case "train" => trainEval.f1
      case _ => testEval.f1
    }
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderLabelDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderLabelDomain, is)
    HeaderLabelDomain.freeze()
    println("HeaderLabelDomain: " + HeaderLabelDomain.categories.mkString(", "))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    println(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    BinarySerializer.deserialize(model, is)
    println(s"model sparsity: ${model.sparsity}")
    is.close()
  }
}

object TrainHeaderTagger extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val f1 = opts.dataSet.value match {
      case "grobid" => trainGrobid(opts)
      case _ => trainDefault(opts)
    }
    if(opts.trainFile.wasInvoked) TestHeaderTagger.main(args)
    f1
  }

  def trainDefault(opts: HeaderTaggerOpts): Double = ???

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    import edu.umass.cs.iesl.paperheader.load.LoadGrobid
    implicit val random = new scala.util.Random
    val params = new HyperParams(opts)
    println(params)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new HeaderTagger(lexicon)
    val allData = LoadGrobid.fromFilename(opts.trainFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)
    println("using labels: " + HeaderLabelDomain.categories.mkString(", "))

    val shuff = random.shuffle(allData)
    val n = allData.length.toDouble
    val trainPart = math.floor(0.9 * n).toInt
    val trainingData = shuff.take(trainPart)
    val devData = shuff.drop(trainPart)

    //    val trainPortion = (allData.length.toDouble * opts.trainPortion.value).floor.toInt
    //    val testPortion = (allData.length.toDouble * (if(opts.testPortion.wasInvoked) opts.testPortion.value else 1.0-opts.trainPortion.value)).floor.toInt
    //    val trainingData = allData.take(trainPortion)
    //    val devData = allData.drop(trainPortion).take(testPortion)

    val testData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)

    println(s"training data: ${trainingData.length} docs, ${trainingData.flatMap(_.tokens).length} tokens")
    println(s"dev data: ${devData.length} docs, ${devData.flatMap(_.tokens).length} tokens")

    trainingData.head.tokens.take(5).foreach { t => println(s"${t.string} ${t.attr[HeaderLabel].categoryValue}")}

    /* load brown clusters */
    if (opts.brownClusters.wasInvoked) {
      println(s"Reading brown cluster file: ${opts.brownClusters.value}")
      for (line <- scala.io.Source.fromFile(opts.brownClusters.value).getLines()) {
        val splitLine = line.split("\t")
        TokenFeatures.clusters(splitLine(1)) = splitLine(0)
      }
    }

    val f1 = tagger.train(trainingData, devData, params)
    if (opts.saveModel.value) {
      println(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    f1
  }
}

object OptimizeCitationModel {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.saveModel.setValue(false)
    opts.writeEvals.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 10))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 10))
    val rate = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-4, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 1))

    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.saveModel.setValue(true)
    opts.writeEvals.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}

object TestHeaderTagger {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => processGrobid(opts)
      case _ => processDefault(opts)
    }
  }
  def processDefault(opts: HeaderTaggerOpts): Unit = throw new Exception("not yet implemented")

  def processGrobid(opts: HeaderTaggerOpts): Unit = {
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val trainer = new HeaderTagger(lexicon, opts.modelFile.value)
    println(s"loading file: ${opts.testFile.value} with grobid features? ${opts.useGrobidFeatures.value}")
    val testingData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)
    testingData.foreach(doc => trainer.addFeatures(doc))
    val labels = testingData.flatMap(_.tokens).map(_.attr[HeaderLabel])
    testingData.foreach(trainer.process)
    val segEval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, labels.toIndexedSeq)
    println(segEval)
    if (opts.outputTagged.wasInvoked) {
      val outputFname = opts.outputTagged.value
      println(s"writing tagged output to $outputFname")
      val writer = new PrintWriter(outputFname, "utf-8")
      testingData.foreach { doc => {
        doc.tokens.foreach { token => {
          val label = token.attr[HeaderLabel]
          writer.println(s"${token.string}\t${label.target.categoryValue}\t${label.categoryValue}")
        }}
        writer.println()
      }}
      writer.close()
    }
  }
}

case class HyperParams(opts: HeaderTaggerOpts) {
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val learningRate = opts.learningRate.value
  val delta = opts.delta.value
  val iters = opts.numIterations.value
  val eval = opts.hyperparamEval.value
  override def toString(): String = s"HyperParams(l1=$l1 l2=$l2 rate=$learningRate delta=$delta)"
}

class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
  /* data */
  val trainFile = new CmdOption("train-file", "", "STRING", "Filename(s) from which to read training data")
  val trainDir = new CmdOption("train-dir", "", "STRING", "directory of train files")
  val devFile = new CmdOption("dev-file", "", "STRING", "filename of dev set")
  val devDir = new CmdOption("dev-dir", "", "STRING", "directory of dev files")
  val testFile = new CmdOption("test-file", "", "STRING", "Filename(s) from which to read test data")
  val testDir = new CmdOption("test-dir", "", "STRING", "directory of test files")
  val dataSet = new CmdOption("data-set", "", "STRING", "which data set to use (grobid, fullpaper)")

  /* hyperparameters */
  val l1 = new CmdOption("l1", 1e-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 1e-5, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.1, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")
  val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
  val hyperparamEval = new CmdOption("hyperparam-eval", "dev", "STRING", "On what to eval hyperparams (train|dev)")

  /* serialization */
  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "serialize the model?")
  val modelFile = new CmdOption("model-file", "HeaderTagger.factorie", "STRING", "filename of serialized model")
  val outputTagged = new CmdOption("output-tagged", "", "STRING", "tagged file output filename")

  /* misc other knobs */
  val brownClusters = new CmdOption("brown-clusters", "", "STRING", "path to brown clusters")
  val rootDir = new CmdOption("root-dir", "", "STRING", "project root")
  val outputDir = new CmdOption("output-dir", "", "STRING", "directory to write evaluations to")
  val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
  val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
  val bilou = new CmdOption("bilou", false, "BOOLEAN", "use bilou encoding?")
  val nThreads = new CmdOption("threads", 1, "INT", "Number of threads to use during training")

}

