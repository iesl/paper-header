package edu.umass.cs.iesl.paperheader.tagger

import java.io.{BufferedInputStream, BufferedOutputStream}
import cc.factorie._
import cc.factorie.model._
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.util.{JavaHashSet, JavaHashMap, BinarySerializer}
import cc.factorie.variable._
import cc.factorie.app.chain._
import edu.umass.cs.iesl.paperheader.load._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.util.Random._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.chain.SegmentEvaluation

/**
 * Created by kate on 9/25/14.
 */

class HeaderTagger(val url:java.net.URL=null, useFormatting:Boolean=false) extends DocumentAnnotator {

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }
  class HeaderTaggerCRFModel extends ChainModel[BilouHeaderTag, FeatureVariable, Token](
    HeaderTagDomain,
    FeatureDomain,
    l => l.token.attr[FeatureVariable],
    l => l.token,
    t => t.attr[BilouHeaderTag]
  )

  val model = new HeaderTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BilouHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[Sentence])//, classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouHeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[BilouHeaderTag]))
      document.tokens.map(token => token.attr += new BilouHeaderTag(token, "I-abstract"))
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    process1(document)
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => HeaderTagDomain.spanList(section)))
    document
  }

  def process1(doc:Document): Unit = {
    if (doc.tokens.size > 0) {
      if (!doc.tokens.head.attr.contains(classOf[BilouHeaderTag])) {
        doc.tokens.foreach(t => t.attr += new BilouHeaderTag(t, "I-abstract"))
      }
      for (sentence <- doc.sentences if sentence.tokens.size > 0) {
        val vars = sentence.tokens.map(_.attr[BilouHeaderTag]).toSeq
        model.maximize(vars)(null)
      }
    }
  }


  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable]
    val tokenSeq = doc.sentences.flatMap(_.tokens).toSeq
    tokenSeq.foreach(t => {
      t.attr += new FeatureVariable(t)
      vf(t) ++= TokenFeatures(t)
    })
    LexiconTagger.tagText(tokenSeq, vf)
    for (sentence <- doc.sentences) {
      addNeighboringFeatureConjunctions(sentence.tokens.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
      val sfeats = SentenceFeatures(sentence)
      if (sfeats.length > 0) sentence.tokens.foreach(t => vf(t) ++= sfeats)
      // ambiguity classes
      for (token <- sentence.tokens) {
        val amb0 = WordData.ambiguityClasses.getOrElse(token.lemmaStr, null)
        if (amb0 != null) vf(token) += s"AMB@0=$amb0"
        for (i <- List(1, 2)) {
          val next = token.next(i)
          if (next != null) {
            val amb = WordData.ambiguityClasses.getOrElse(token.lemmaStr, null)
            if (amb != null) vf(token) += s"AMB@$i=$amb"
          }
        }
      }
    }
  }



  def train(trainDocs:Seq[Document], testDocs:Seq[Document], params: HyperParams)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq

    println("computing WordData...")
    WordData.computeWordFormsByDocFreq(trainDocs)
    WordData.computeAmbiguityClasses(trainDocs)

    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.par.foreach(addFeatures)

    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)

    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq))
      }
      else println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity (model1)")
    }

    val vars = for (td <- trainDocs; sentence <- td.sentences if sentence.length > 0) yield sentence.tokens.map(_.attr[LabeledBilouHeaderTag])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    val optimizer = new AdaGradRDA(l1=params.l1, l2=params.l2, delta=params.delta, rate=params.learningRate, numExamples=examples.length)
    println("training...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate, useParallelTrainer=false)
    trainDocs.foreach(process)
    testDocs.foreach(process)

    println("FINAL:")
    val eval = new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq)
    println(eval)
    eval.f1
  }

  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    FeatureDomain.freeze()
    println("Found model")
  }
  else {
    println("model not found")
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    BinarySerializer.serialize(WordData.ambiguityClasses, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    BinarySerializer.deserialize(WordData.ambiguityClasses, is)
    is.close()
  }
}

object HeaderTaggerUtils {
  def collectStats(docs: Seq[Document]): Unit = {
    var tokenCount = 0
    var sentenceCount = 0
    val tagCounts = new scala.collection.mutable.HashMap[String, Int]()
    docs.foreach(doc => {
      for (sentence <- doc.sentences if sentence.length > 0) {
        sentenceCount += 1
        sentence.tokens.foreach(t => {
          assert(t.attr.contains(classOf[LabeledBilouHeaderTag]), s"token with null tag ${t.string}")
          val tag = t.attr[LabeledBilouHeaderTag].categoryValue.split("-")(1)
          if (!tagCounts.contains(tag)) tagCounts(tag) = 1
          else tagCounts(tag) += 1
          tokenCount += 1
        })
      }
    })
    val toksPerDoc = tokenCount.toDouble / docs.length.toDouble
    println(s"total docs: ${docs.length} ; avg toks per doc: $toksPerDoc")
    val toksPerSent = tokenCount.toDouble / sentenceCount.toDouble
    println(s"total sentences: $sentenceCount ; avg toks per sentence: $toksPerSent")
    println(s"total tokens: $tokenCount")
    tagCounts.keys.foreach(k => {
      println(s"$k : ${tagCounts(k)} ${tagCounts(k).toDouble / tokenCount.toDouble}")
    })
  }
  def loadDevAndTrainData(dir: String, dataSet: String = "grobid"): (Seq[Document], Seq[Document]) = {
    dataSet match {
      case "grobid" =>
        val docs = LoadGrobid.loadDataFromDir(dir)
        val trainPortion = (docs.length * 0.8).floor.toInt
        val trainDocs = docs.take(trainPortion)
        val devDocs = docs.drop(trainPortion)
        (trainDocs, devDocs)
      case _ => throw new Exception("TODO Exception (Not Yet Implemented)")
    }
  }
  def loadDataSets(grobidDir: String = "", fpFile: String = "", dataSet: String = "all"): (Seq[Document], Seq[Document], Seq[Document]) = {
    dataSet match {
      case "grobid" =>
        val (trd, dd, td) = LoadGrobid.loadDataSetsFromDir(grobidDir)
        (shuffle(trd), shuffle(dd), shuffle(td))
      case "fullpaper" =>
        val (trd, dd, td) = LoadTSV.loadDataSets(fpFile)
        (shuffle(trd), shuffle(dd), shuffle(td))
      case "all" =>
        val (grobidTrain, grobidDev, grobidTest) = LoadGrobid.loadDataSetsFromDir(grobidDir)
        val (fpTrain, fpDev, fpTest) = LoadTSV.loadDataSets(fpFile, BILOU=true)
        val trainDocs = shuffle(grobidTrain ++ fpTrain)
        val devDocs = shuffle(grobidDev ++ fpDev)
        val testDocs = shuffle(grobidTest ++ fpTest)
        (trainDocs, devDocs, testDocs)
    }
  }
}

case class HyperParams(opts: HeaderTaggerOpts) {
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val learningRate = opts.learningRate.value
  val delta = opts.delta.value
}

class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "HeaderTagger.factorie", "STRING", "Filename for the model (saving a trained model or reading a running model.")
  val model = new CmdOption("model", "HeaderTagger.factorie", "STRING", "filename of serialized model")
  val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize at all")
  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data")
  val trainDir = new CmdOption("train-dir", "", "STRING", "directory of train files")
  val dev = new CmdOption("dev", "", "STRING", "filename of dev set")
  val devDir = new CmdOption("dev-dir", "", "STRING", "directory of dev files")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data")
  val testDir = new CmdOption("test-dir", "", "STRING", "directory of test files")
  val dataSet = new CmdOption("data-set", "", "STRING", "which data set to use (grobid, fullpaper, all)")
  val l1 = new CmdOption("l1", 1e-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 1e-5, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.8515541191715452, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")
}


object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger

    val (trainDocs, devDocs) = opts.dataSet.value match {
      case "grobid" => (LoadGrobid.loadDataFromDir(opts.trainDir.value), LoadGrobid.loadDataFromDir(opts.devDir.value))
      case "fp" => (LoadTSV.loadTSV(opts.train.value, BILOU=true), LoadTSV.loadTSV(opts.dev.value, BILOU=true))
      case "all" =>
        val train = shuffle(LoadGrobid.loadDataFromDir(opts.trainDir.value) ++ LoadTSV.loadTSV(opts.train.value, BILOU=true))
        val dev = shuffle(LoadGrobid.loadDataFromDir(opts.devDir.value) ++ LoadTSV.loadTSV(opts.dev.value, BILOU=true))
        (train, dev)
      case _ => throw new Exception("TODO")
    }

    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}, delta=${opts.delta.value}")
    println("TRAIN:")
    HeaderTaggerUtils.collectStats(trainDocs)
    println("DEV:")
    HeaderTaggerUtils.collectStats(devDocs)

    val hyperparams = HyperParams(opts)

    val result = tagger.train(trainDocs, devDocs, hyperparams)

    val testDocs = opts.dataSet.value match {
      case "grobid" => LoadGrobid.loadDataFromDir(opts.testDir.value)
      case "fp" => LoadTSV.loadTSV(opts.test.value, BILOU=true)
      case "all" => shuffle(LoadGrobid.loadDataFromDir(opts.testDir.value) ++ LoadTSV.loadTSV(opts.test.value, BILOU=true))
      case _ => throw new Exception("TODO")
    }
    println("TEST:")
    HeaderTaggerUtils.collectStats(testDocs)

    val testLabels = testDocs.flatMap(_.sentences).flatMap(_.tokens).map(_.attr[LabeledBilouHeaderTag])
    testDocs.foreach(tagger.process)
    println("test evaluation:")
    println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq))

    if (opts.serialize.value){
      val fname = if (opts.saveModel.wasInvoked) opts.saveModel.value else "HeaderTagger.factorie"
      println(s"serializing model to: $fname")
      tagger.serialize(new FileOutputStream(fname))
    }
    result
  }
}

object HeaderTaggerOptimizer {
  def main(args: Array[String]): Unit = {
    import cc.factorie.util.{HyperParameter, LogUniformDoubleSampler, HyperParameterSearcher}
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.serialize.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-5, 10))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-5, 10))
    val qs = new cc.factorie.util.QSubExecutor(16, "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2), qs.execute, 100, 90, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value + " best lr: " + opts.learningRate.value + " best delta: " + opts.delta.value)
    println("Running best configuration...")
    opts.serialize.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.unParse.toArray), 1.hours)
    println("Done.")
  }
}

object HeaderTaggerTester {
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger(url = new java.net.URL("file://" + opts.model.value))
    //    val (_, _, testDocs) = HeaderTaggerUtils.loadDataSets(
    //      grobidDir = if (opts.dataDir.wasInvoked) opts.dataDir.value else "",
    //      fpFile = if (opts.data.wasInvoked) opts.data.value else "",
    //      dataSet = opts.dataSet.value
    //    )
    //    if (opts.dataSet.value != "grobid") throw new Exception("TODO Exception (Not Yet Implemented)")
    //    val testDocs = LoadGrobid.loadDataFromDir(opts.dataDir.value)
    val testDocs = opts.dataSet.value match {
      case "grobid" => LoadGrobid.loadDataFromDir(opts.testDir.value)
//      case "fp" => LoadTSV.loadTSV(opts.data.value, BILOU=true)
      case _ => throw new Exception("TODO Exception")
    }
    HeaderTaggerUtils.collectStats(testDocs)
    val labels = new scala.collection.mutable.ListBuffer[LabeledBilouHeaderTag]()
    testDocs.foreach(doc => {
      val tokens = doc.sentences.flatMap(_.tokens)
      labels ++= tokens.map(_.attr[LabeledBilouHeaderTag])
      tagger.process(doc)
    })
    println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, labels.toIndexedSeq))
  }
}

object OneTimeThing {
  def writeOut(docs: Seq[Document], filename: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(filename))
    docs.filter(doc => doc.tokenCount > 0).foreach(doc => {
      pw.write("# somedoc\n")
      for (sentence <- doc.sentences if sentence.tokens.size > 0; token <- sentence.tokens) {
        assert(token.attr.contains(classOf[LabeledBioHeaderTag]), s"token ${token.string} with no label")
        val label = token.attr[LabeledBioHeaderTag].categoryValue
        val string = token.string
        pw.write(s"$label\t$string\n")
      }
      pw.write("\n")
    })
    pw.close()
  }
  def findUniqTags(docs: Seq[Document]): Unit = {
    val tagSet = new scala.collection.mutable.HashSet[String]()
    for (doc <- docs; sentence <- doc.sentences if sentence.tokens.size > 0; token <- sentence.tokens) {
      assert(token.attr.contains(classOf[LabeledBioHeaderTag]), "token with no label")
      val label = token.attr[LabeledBioHeaderTag].categoryValue.substring(2)
      tagSet += label
    }
    tagSet.toList.foreach(println)
  }
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val allDocs = LoadTSV.loadTSV(opts.train.value)
    //    findUniqTags(allDocs)
    ////    HeaderTaggerUtils.collectStats(allDocs)
    val trainSeg = (allDocs.length * 0.7).floor.toInt
    val trainDocs = allDocs.take(trainSeg)
    val restDocs = allDocs.drop(trainSeg)
    val devSeg = (restDocs.length * 0.4).floor.toInt
    val devDocs = restDocs.take(devSeg)
    val testDocs = restDocs.drop(devSeg)
    writeOut(trainDocs, "fullpaper-train.tsv")
    writeOut(devDocs, "fullpaper-dev.tsv")
    writeOut(testDocs, "fullpaper-test.tsv")
  }
}