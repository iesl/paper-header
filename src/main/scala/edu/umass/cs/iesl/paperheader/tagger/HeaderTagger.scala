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
 *
 * TODO just label sentences instead of tokens?
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

  object FeatureDomain2 extends CategoricalVectorDomain[String]
  class FeatureVariable2(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain2
    override def skipNonCategories = true
  }
  class HeaderTaggerCRFModel2 extends ChainModel[BilouHeaderTag, FeatureVariable2, Token](
    HeaderTagDomain,
    FeatureDomain2,
    l => l.token.attr[FeatureVariable2],
    l => l.token,
    t => t.attr[BilouHeaderTag]
  )

  //  object HeaderSpanFeaturesDomain extends CategoricalDomain[String]
  //  class HeaderSpanFeatures(val span: TokenSpan) extends BinaryFeatureVectorVariable[String] {
  //    def domain = HeaderSpanFeaturesDomain
  //    override def skipNonCategories = true
  //  }
  //  def hasPrevSent(s: TokenSpan): Boolean = s.indexInSection > 1
  //  def prevSentence(s: TokenSpan): Sentence = {
  //    if (hasPrevSent(s)) {
  //      val sents = s.document.sentences.toIndexedSeq
  //      sents(s.indexInSection - 1)
  //    } else null
  //  }
  //  class HeaderSpanCRFModel extends TemplateModel with Parameters {
  //    // factor between span label and observed token span
  //    val localTemplate = new DotTemplateWithStatistics2[HeaderTagSpanLabel, HeaderSpanFeatures] {
  //      factorName = "observation"
  //      val weights = Weights(new DenseTensor1(HeaderTagDomain.size, HeaderSpanFeaturesDomain.dimensionSize))
  //      def unroll1(label: HeaderTagSpanLabel) = Factor(label, label.span.attr[HeaderSpanFeatures])
  //      def unroll2(tf: HeaderSpanFeatures) = Factor(tf.span.attr[HeaderTagSpanLabel], tf)
  //    }
  //    // transition factors between two successive span labels
  //    val transitionTemplate = new DotTemplateWithStatistics2[HeaderTagSpanLabel, HeaderTagSpanLabel] {
  //      factorName = "markov"
  //      val weights = Weights(new DenseTensor2(HeaderTagDomain.size, HeaderTagDomain.size))
  //      def unroll1(label: HeaderTagSpanLabel) = if (hasPrevSent(label.span)) Factor()
  //    }
  //  }

  val model = new HeaderTaggerCRFModel
  val model2 = new HeaderTaggerCRFModel2
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
    val alreadyHadFeatures2 = document.hasAnnotation(classOf[FeatureVariable2])
    if (!alreadyHadFeatures2) addSecondaryFeatures(document)
    process2(document)
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => HeaderTagDomain.spanList(section)))
    document
  }

  var added = 0

  def process1(doc:Document): Unit = {
    if (doc.tokens.size > 0) {
      //      TODO should this be setRandomly or "O", or is it okay to set to I-abstract since it presumably has by far the highest prior prob?
      if (!doc.tokens.head.attr.contains(classOf[BilouHeaderTag])) {
        doc.tokens.foreach(t => t.attr += new BilouHeaderTag(t, "I-abstract"))
        added += 1
      }
      for (sentence <- doc.sentences if sentence.tokens.size > 0) {
        val vars = sentence.tokens.map(_.attr[BilouHeaderTag]).toSeq
        model.maximize(vars)(null)
      }
    }
  }

  def process2(doc:Document): Unit = {
    if (doc.tokenCount > 0) {
      //      TODO should this be setRandomly or "O", or is it okay to set to I-abstract since it presumably has by far the highest prior prob?
      //      if (!doc.tokens.head.attr.contains(classOf[BilouHeaderTag])) doc.tokens.foreach(t => t.attr += new BilouHeaderTag(t, "I-abstract"))
      for (sentence <- doc.sentences if sentence.tokens.size > 0) {
        val vars = sentence.tokens.map(_.attr[BilouHeaderTag]).toSeq
        model2.maximize(vars)(null)
      }
    }
  }


  def tokens(d:Document): Seq[Token] = d.sections.flatMap(_.tokens).toSeq

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable]
    val tokenSeq = tokens(doc)
    tokenSeq.foreach(t => {
      t.attr += new FeatureVariable(t)
      vf(t) ++= TokenFeatures(t)
    })
    //    LexiconTagger.tagText(tokenSeq, vf)
    for (sentence <- doc.sentences) {
      addNeighboringFeatureConjunctions(sentence.tokens.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
      val sfeats = SentenceFeatures(sentence)
      if (sfeats.length > 0) sentence.tokens.foreach(t => vf(t) ++= sfeats)
      // ambiguity classes
      for (token <- sentence.tokens) {
        //        for (i <- List(1, 2)) {
        //          val prev = token.prev(i)
        //          if (prev != null && prev.attr.contains(classOf[BilouHeaderTag])) vf(token) += s"PP@-$i=${prev.attr[BilouHeaderTag].categoryValue}"
        //        }
        val amb0 = WordData.ambiguityClasses.getOrElse(WordData.lemmatize(token), null)
        if (amb0 != null) vf(token) += s"AMB@0=$amb0"
        for (i <- List(1, 2)) {
          val next = token.next(i)
          if (next != null) {
            val amb = WordData.ambiguityClasses.getOrElse(WordData.lemmatize(next), null)
            if (amb != null) vf(token) += s"AMB@$i=$amb"
          }
        }
      }
    }

  }



  def addSecondaryFeatures(doc: Document): Unit = {
    doc.annotators(classOf[FeatureVariable2]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable2]
    for (sentence <- doc.sentences if sentence.tokens.size > 0) {
      val tokenSeq = sentence.tokens
      tokenSeq.foreach(t => {
        t.attr += new FeatureVariable2(t)
        vf(t) ++= t.attr[FeatureVariable].activeCategories
        if (t.hasPrev) vf(t) += s"PP@-1=${t.prev.attr[BilouHeaderTag].categoryValue}"
        if (t.hasPrev(2)) vf(t) += s"PP@-2=${t.prev(2).attr[BilouHeaderTag].categoryValue}"
      })
    }
  }

  def evaluate(trainDocs: Seq[Document], testDocs: Seq[Document], useModel2:Boolean=false): Unit = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq
    val trainLabels = labels(trainDocs); val testLabels = labels(testDocs)
    if (useModel2) trainDocs.foreach(process2) else trainDocs.foreach(process1)
    println("")
    println("Train accuracy (overall): "+objective.accuracy(trainLabels))
    println("Training:")
    println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, trainLabels.toIndexedSeq))
    if (testDocs.nonEmpty) {
      if (useModel2) testDocs.par.foreach(process2) else testDocs.par.foreach(process1)
      println("Test  accuracy (overall): "+objective.accuracy(testLabels))
      println("Testing:")
      println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq))
    }
    if (useModel2) println(model2.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model2.parameters.tensors.sumInts(_.length)+" sparsity (model2)")
    else println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity (model1)")
    println("")
    val firstToken = trainDocs(0).sentences.flatMap(_.tokens).take(1)
    firstToken.foreach(t => println("attr: " + t.attr.toString()))
    firstToken.foreach(t => println(t.attr[BilouHeaderTag]))
    firstToken.foreach(t => println(t.attr[LabeledBilouHeaderTag]))
    println("")

  }

  def printFeatures(doc: Document, n: Int = 4): Unit = {
    doc.sentences.flatMap(_.tokens).take(n).foreach(t => {
      if (t.attr.contains(classOf[FeatureVariable2])) {
        println(t.attr[FeatureVariable2].activeCategories.sorted.mkString(", "))
      } else {
        println(t.attr[FeatureVariable].activeCategories.sorted.mkString(", "))
      }
      println("")
    })
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1, delta:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq

    println("computing WordData...")
    WordData.computeWordFormsByDocFreq(trainDocs)
    WordData.computeAmbiguityClasses(trainDocs)

    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.par.foreach(addFeatures)
    printFeatures(trainDocs(0))

    val testLabels = labels(testDocs)

    val vars = for (td <- trainDocs; sentence <- td.sentences if sentence.length > 0) yield sentence.tokens.map(_.attr[LabeledBilouHeaderTag])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    println("training model 1...")
    Trainer.onlineTrain(model.parameters, examples, optimizer=new AdaGrad(rate=lr, delta=delta) with ParameterAveraging, useParallelTrainer=false)
    trainDocs.foreach(process1)
    testDocs.foreach(process1)
    println("FINAL 1:")
    val eval = new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq)
    println(eval)
    trainDocs(0).tokens.take(1).foreach(t => {
      println(t.attr.toString())
      assert(t.attr.contains(classOf[BilouHeaderTag]), "token with no BilouHeaderTag")
    })
    println("ADDED: " + added)

    println("adding secondary features...")
    trainDocs.foreach(addSecondaryFeatures)
    FeatureDomain2.freeze()
    testDocs.par.foreach(addSecondaryFeatures)
    printFeatures(trainDocs(0), 10)
    printFeatures(trainDocs(1), 10)

    val vars2 = for (td <- trainDocs; sentence <- td.sentences if sentence.length > 0) yield sentence.tokens.map(_.attr[LabeledBilouHeaderTag])
    val examples2 = vars2.map(v => new model2.ChainLikelihoodExample(v.toSeq))
    //    def evaluate2(): Unit = evaluate(trainDocs, testDocs, useModel2=true)
    println("training model 2...")
    //    optimize.Trainer.onlineTrain(model2.parameters, examples2, optimizer=optimizer, evaluate=evaluate2)
    Trainer.onlineTrain(model2.parameters, examples2, optimizer=new AdaGrad(rate=lr, delta=delta) with ParameterAveraging, useParallelTrainer=false)
    trainDocs.foreach(process)
    testDocs.foreach(process)
    val eval2 = new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq)
    println("FINAL 2:")
    println(eval2)
    println("")
    val firstToken2 = trainDocs(0).sentences.flatMap(_.tokens).take(1)
    firstToken2.foreach(t => println("attr: " + t.attr.toString()))
    firstToken2.foreach(t => println(t.attr[BilouHeaderTag]))
    firstToken2.foreach(t => println(t.attr[LabeledBilouHeaderTag]))
    println("")
    eval2.f1

  }


  def processUsingModel(document: Document, m: HeaderTaggerCRFModel): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    val labels = document.sections.flatMap(_.tokens).toSeq.map(_.attr[BilouHeaderTag])
    m.maximize(labels)(null)
    if (!alreadyHadFeatures) {
      document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable]
    }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => HeaderTagDomain.spanList(section)))
    document
  }

  def trainKFold(trainDocs:Seq[Document], testDocs:Seq[Document],
                 k: Int = 10, l1:Double=0.1, l2:Double=0.1, lr:Double=0.1, delta:Double=0.1, iters:Int=5)(implicit random: scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val allTestLabels = labels(testDocs)
    val kPortion = (trainDocs.length / k).toDouble
    val groups = trainDocs.grouped(kPortion.floor.toInt).toIndexedSeq
    val models = new ArrayBuffer[HeaderTaggerCRFModel]()
    val accuracies = Array.fill(groups.length-1)(0.0)
    for (i <- 0 until groups.length-1) {
      val tmpModel = new HeaderTaggerCRFModel
      val heldout = groups(i)
      val testLabels = labels(heldout)
      val rest = (0 until i-1).map(j => groups(j)) ++ (i+1 until groups.length).map(j => groups(j))
      val restDocs = rest.flatten
      val trainLabels = labels(restDocs)
      val examples = restDocs.map(doc => new tmpModel.ChainLikelihoodExample(doc.sections.flatMap(_.tokens).map(_.attr[LabeledBilouHeaderTag])))
      val optimizer = new optimize.AdaGradRDA(delta=delta, rate=lr, l1=l1, l2=l2, numExamples=examples.length)
      def evaluate(): Unit = {
        restDocs.foreach(doc => processUsingModel(doc, tmpModel))
        println("Train accuracy (overall): "+objective.accuracy(trainLabels))
        println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, trainLabels.toIndexedSeq))
        heldout.par.foreach(doc => processUsingModel(doc, tmpModel))
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq))
      }
      println(s"training (iter=$i)...")
      optimize.Trainer.onlineTrain(tmpModel.parameters, examples, optimizer=optimizer, evaluate=evaluate, maxIterations=iters)
      heldout.par.foreach(doc => processUsingModel(doc, tmpModel))
      accuracies(i) = new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq).f1
      models += tmpModel
    }
    val bestModel: HeaderTaggerCRFModel = models.toSeq.zip(accuracies).maxBy(_._2)._1
    testDocs.foreach(doc => processUsingModel(doc, bestModel))
    new SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, allTestLabels.toIndexedSeq).f1
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
    BinarySerializer.serialize(FeatureDomain2.dimensionDomain, is)
    BinarySerializer.serialize(model2, is)
    BinarySerializer.serialize(WordData.ambiguityClasses, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    BinarySerializer.deserialize(FeatureDomain2.dimensionDomain, is)
    BinarySerializer.deserialize(model2, is)
    BinarySerializer.deserialize(WordData.ambiguityClasses, is)
    is.close()
  }

  def lines(d:Document): LineBuffer = if (d.attr[LineBuffer] ne null) d.attr[LineBuffer] else null.asInstanceOf[LineBuffer]
  def addFormattingFeatures(doc:Document): Unit = {
    val vf = (t:Token) => t.attr[FeatureVariable]
    /* Line spacing */
    doc.attr[LineBuffer].blocks(0).tokens.foreach(t => {
      vf(t) += "YDIFF=0"
      //      vf(t) += "XDIFF=0"
    })
    doc.attr[LineBuffer].blocks.sliding(2).foreach(pair => {
      if (pair.length == 2) {
        val line1: Line = pair(0)
        val line2: Line = pair(1)
        val verticalDiff = Math.abs(line1.ypos - line2.ypos)
        line2.tokens.foreach(t => vf(t) += s"YDIFF=$verticalDiff")
        //        val xDiff = Math.abs(line1.tokens(0).attr[FormatInfo].xpos - line2.tokens(0).attr[FormatInfo].xpos)
        //        line2.tokens.foreach(t => vf(t) += s"XDIFF=$xDiff")
      }
    })
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

class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "HeaderTagger.factorie", "STRING", "Filename for the model (saving a trained model or reading a running model.")
  val model = new CmdOption("model", "HeaderTagger.factorie", "STRING", "filename of serialized model")
  val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize at all")

  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data")
  val data = new CmdOption("data", "", "STRING", "filename of all data")
  val dataDir = new CmdOption("data-dir", "", "STRING", "directory of data")

  val dataSet = new CmdOption("data-set", "", "STRING", "which data set to use (grobid, fullpaper, all)")

  val l1 = new CmdOption("l1", 1.424388380418031E-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 0.06765909781125444, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.8515541191715452, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")

  val useFormatting = new CmdOption("use-formatting", false, "BOOLEAN", "use formatting features")
}

object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {

  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger

    //    val (trainDocs, devDocs, _) = HeaderTaggerUtils.loadDataSets(
    //      grobidDir = if (opts.dataDir.wasInvoked) opts.dataDir.value else "",
    //      fpFile = if (opts.data.wasInvoked) opts.data.value else "",
    //      dataSet = opts.dataSet.value
    //    )
    val (trainDocs, devDocs) = HeaderTaggerUtils.loadDevAndTrainData(opts.dataDir.value, opts.dataSet.value)

    // print some statistics
    println(s"using ${trainDocs.length} training docs with ${trainDocs.map(_.tokenCount).sum} tokens total")
    println(s"using ${devDocs.length} dev docs with ${devDocs.map(_.tokenCount).sum} tokens total")
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}, delta=${opts.delta.value}")
    HeaderTaggerUtils.collectStats(trainDocs)

    val result = tagger.train(trainDocs, devDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value, delta=opts.delta.value)

    println(s"FINAL RESULT: f1 = $result")

    if (opts.serialize.value){
      val fname = if (opts.saveModel.wasInvoked) opts.saveModel.value else "HeaderTagger.factorie"
      println(s"saving model to: $fname")
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
    //    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-5, 10))
    //    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-5, 10))
    val lr = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-4, 10))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 10))
    val qs = new cc.factorie.util.QSubExecutor(16, "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(lr, delta), qs.execute, 100, 90, 60)
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
      case "grobid" => LoadGrobid.loadDataFromDir(opts.dataDir.value)
      case "fp" => LoadTSV.loadTSV(opts.data.value, BILOU=true)
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

