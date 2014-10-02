package edu.umass.cs.iesl.paperheader

import java.io.{BufferedInputStream, BufferedOutputStream, File}

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.{BinarySerializer, CubbieConversions}
import cc.factorie.variable._
import cc.factorie.app.chain._
import scala.collection.mutable.ArrayBuffer
import Features._
import java.io._

/* Sample stdin output:

evaluate(): processing 747 train docs...
  guess=B-author true=B-author FeatureVariable([always-on],SIMPLIFIED=,no-digits,SHAPE2=,SHAPE3=)
 Chungki guess=I-author true=I-author FeatureVariable([always-on],no-digits,SIMPLIFIED=chungki,SHAPE2=Aaa,SHAPE3=Aa,Capitalized)
 Lee guess=I-author true=I-author FeatureVariable([always-on],no-digits,SHAPE2=Aaa,SHAPE3=Aa,Capitalized,SIMPLIFIED=lee,PERSON-FIRST-HIGH,PERSON-LAST-HIGH)
 James guess=I-author true=I-author FeatureVariable([always-on],no-digits,SHAPE2=Aaa,SHAPE3=Aa,Capitalized,SIMPLIFIED=james,PERSON-FIRST-HIGHEST,PERSON-LAST-MEDIUM)
 E. guess=I-author true=I-author FeatureVariable([always-on],no-digits,SIMPLIFIED=Initial,SHAPE2=A.,SHAPE3=A.,Acronym,ContainsPunc,Initial)
* Burns guess=I-author true=L-author FeatureVariable([always-on],no-digits,SHAPE2=Aaa,SHAPE3=Aa,Capitalized,SIMPLIFIED=burns,PERSON-LAST-HIGH)
* Mostafa guess=I-author true=B-author FeatureVariable([always-on],no-digits,SHAPE2=Aaa,SHAPE3=Aa,Capitalized,SIMPLIFIED=mostafa,PERSON-FIRST-MEDIUM)
 H. guess=I-author true=I-author FeatureVariable([always-on],no-digits,SIMPLIFIED=Initial,SHAPE2=A.,SHAPE3=A.,Acronym,ContainsPunc,Initial)
 Ammar guess=L-author true=L-author FeatureVariable([always-on],no-digits,SHAPE2=Aaa,SHAPE3=Aa,Capitalized,SIMPLIFIED=ammar,PERSON-FIRST-MEDIUM,PERSON-LAST-HIGH)
  guess=B-email true=B-email FeatureVariable([always-on],SIMPLIFIED=,no-digits,SHAPE2=,SHAPE3=)
Train accuracy 0.8638281924687556
OVERALL  f1=0.594790 p=0.787670 r=0.477791 (tp=8573 fp=2311 fn=9370 true=17943 pred=10884) acc=0.863828 (106028/122742)
abstract f1=0.143306 p=0.464006 r=0.084738 (tp=593 fp=685 fn=6405 true=6998 pred=1278)
address  f1=0.733333 p=0.806034 r=0.672662 (tp=561 fp=135 fn=273 true=834 pred=696)
affiliation f1=0.678481 p=0.730245 r=0.633570 (tp=804 fp=297 fn=465 true=1269 pred=1101)
author   f1=0.673431 p=0.720488 r=0.632143 (tp=531 fp=206 fn=309 true=840 pred=737)
date     f1=0.957523 p=0.948645 r=0.966570 (tp=665 fp=36 fn=23 true=688 pred=701)
degree   f1=0.830476 p=0.934286 r=0.747429 (tp=654 fp=46 fn=221 true=875 pred=700)
email    f1=0.963230 p=0.966715 r=0.959770 (tp=668 fp=23 fn=28 true=696 pred=691)
intro    f1=0.941432 p=0.932665 r=0.950365 (tp=651 fp=47 fn=34 true=685 pred=698)
keyword  f1=0.831044 p=0.870504 r=0.795007 (tp=605 fp=90 fn=156 true=761 pred=695)
note     f1=0.588107 p=0.722520 r=0.495860 (tp=539 fp=207 fn=548 true=1087 pred=746)
phone    f1=0.951498 p=0.938115 r=0.965268 (tp=667 fp=44 fn=24 true=691 pred=711)
pubnum   f1=0.936474 p=0.931818 r=0.941176 (tp=656 fp=48 fn=41 true=697 pred=704)
title    f1=0.353763 p=0.451303 r=0.290893 (tp=329 fp=400 fn=802 true=1131 pred=729)
web      f1=0.936599 p=0.932568 r=0.940666 (tp=650 fp=47 fn=41 true=691 pred=697)

Test  accuracy 0.8582998454404945
OVERALL  f1=0.570837 p=0.759089 r=0.457402 (tp=2067 fp=656 fn=2452 true=4519 pred=2723) acc=0.858300 (27766/32350)
abstract f1=0.128798 p=0.409222 r=0.076426 (tp=142 fp=205 fn=1716 true=1858 pred=347)
address  f1=0.651515 p=0.737143 r=0.583710 (tp=129 fp=46 fn=92 true=221 pred=175)
affiliation f1=0.657845 p=0.690476 r=0.628159 (tp=174 fp=78 fn=103 true=277 pred=252)
author   f1=0.680412 p=0.698413 r=0.663317 (tp=132 fp=57 fn=67 true=199 pred=189)
date     f1=0.924855 p=0.914286 r=0.935673 (tp=160 fp=15 fn=11 true=171 pred=175)
degree   f1=0.858639 p=0.916201 r=0.807882 (tp=164 fp=15 fn=39 true=203 pred=179)
email    f1=0.953488 p=0.964706 r=0.942529 (tp=164 fp=6 fn=10 true=174 pred=170)
intro    f1=0.921283 p=0.918605 r=0.923977 (tp=158 fp=14 fn=13 true=171 pred=172)
keyword  f1=0.755208 p=0.810056 r=0.707317 (tp=145 fp=34 fn=60 true=205 pred=179)
note     f1=0.611494 p=0.700000 r=0.542857 (tp=133 fp=57 fn=112 true=245 pred=190)
phone    f1=0.941860 p=0.941860 r=0.941860 (tp=162 fp=10 fn=10 true=172 pred=172)
pubnum   f1=0.936782 p=0.936782 r=0.936782 (tp=163 fp=11 fn=11 true=174 pred=174)
title    f1=0.361233 p=0.465909 r=0.294964 (tp=82 fp=94 fn=196 true=278 pred=176)
web      f1=0.924419 p=0.919075 r=0.929825 (tp=159 fp=14 fn=12 true=171 pred=173)

0.93092227 sparsity
FINAL RESULT: f1 = 0.5708367854183927
*/

/**
 * Created by kate on 9/25/14.
 */

// Fuchen Peng (info extraction from headers) ; ask Adam

// TODO how well did the original code do?
//TODO maybe map e.g. abstract --> Section instead of having one big doc section

class HeaderTagger(url:java.net.URL=null) extends DocumentAnnotator {

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BilouHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouHeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    for (token <- document.tokens) {
      // TODO: not sure if this is needed (prevent null ptr exceptions -- not sure why this would happen though)
      if (token.attr[LabeledBilouHeaderTag] eq null) token.attr += new LabeledBilouHeaderTag(token, "O")
      if (token.attr[BilouHeaderTag] eq null) token.attr += new BilouHeaderTag(token, "O")
    }
    for (section <- document.sections) {
      val labels = section.tokens.map(_.attr[BilouHeaderTag]).toSeq
      model.maximize(labels)(null)
    }
    if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable] }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => HeaderTagDomain.spanList(section)))
    document
  }

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }

  class HModel extends ChainModel[BilouHeaderTag, FeatureVariable, Token](HeaderTagDomain, FeatureDomain, l => l.token.attr[FeatureVariable], l => l.token, t => t.attr[BilouHeaderTag])
  val model = new HModel
  val objective = new HammingTemplate[LabeledBilouHeaderTag]



  /*
  FIXME: "  author FeatureVariable(SIMPLIFIED=,no-digits,SHAPE2=,SHAPE3=)" -- seem to be collecting features for blank tokens ?
  TODO: regexTaggers.foreach(_(this))
   */
  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.strings
    import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass

    val tokenSequence = doc.sections.flatMap(_.tokens)
    val vf = (t:Token)=>t.attr[FeatureVariable]

    /* TOKEN FEATURES */
    tokenSequence.foreach(token => {
      val feats = new FeatureVariable(token)
      token.attr += feats
      val rawWord = token.string
      val word = strings.simplifyDigits(rawWord).toLowerCase()
      if (rawWord(0).isUpper) feats += "INITCAP"
      if (rawWord.forall(_.isUpper)) feats += "ALLCAPS"
      if (strings.containsDigitRegex.findAllIn(rawWord).nonEmpty) feats += "CONTAINSDIGITS"
      if (rawWord.forall(c => "\\d".r.findFirstIn("" + c).nonEmpty)) feats += "ALLDIGITS"




      //      val word = token.string.replace(" ", "") // remove spaces
      //      if (newLine.findFirstIn(word).nonEmpty) feats += "NEWLINE"
      //      else {
      //        feats += s"SIMPLIFIED=${simplify(word)}"
      //        feats ++= digitFeatures(word)
      //        feats += s"SHAPE2=${stringShape(word, 2)}"
      //        feats += s"SHAPE3=${stringShape(word, 1)}"
      //        for (m <- regexMatchers; if m(word)) feats += m.name
      //
      //        // TODO (factorie) : StopWords is still an old PhraseLexicon, not a new TriePhraseLexicon
      //        if (cc.factorie.app.nlp.lexicon.StopWords.contains(word.toLowerCase)) feats += "STOPWORD"


    })


    /*
    TODO:  original lexicons missing from factorie  seem to be:
    iesl/{"utexas/UNIVERSITIES", "state-abbreviations", "places"} and possibly:
    "journals",
    "conferences.full",
    "publisher",
    (however these were commented out in timv's code)
     */
    lexicon.iesl.Country.tagText(tokenSequence, vf, "COUNTRY")
    lexicon.iesl.USState.tagText(tokenSequence, vf, "USSTATE")
    lexicon.iesl.Day.tagText(tokenSequence, vf, "DAY")
    lexicon.iesl.Month.tagText(tokenSequence, vf, "MONTH")


    /*
    TODO: original name lexicons missing from factorie seem to be:
    "personname/ssdi.prfirstlow",
    "personname/ssdi.prlastlow",
    "personname/nicknames",
    "personname/namesuffixes"
     */

    /*
    FIXME:
    java.lang.reflect.InvocationTargetException
    ...
    Caused by: java.lang.Error: No file named ssdi/person-first-highest.txt found in classpath for class cc.factorie.app.nlp.lexicon.Lexicon, and no value found in system property cc.factorie.app.nlp.lexicon.Lexicon. To fix this either add a file with the right name to the classpath or set the system property to point to a directory containing the file.
     */
    //    lexicon.ssdi.PersonFirstHighest.tagText(tokenSequence, vf, "PERSON-FIRST-HIGHEST")
    //    lexicon.ssdi.PersonFirstHigh.tagText(tokenSequence, vf, "PERSON-FIRST-HIGH")
    //    lexicon.ssdi.PersonFirstMedium.tagText(tokenSequence, vf, "PERSON-FIRST-MEDIUM")
    //    lexicon.ssdi.PersonLastHighest.tagText(tokenSequence, vf, "PERSON-LAST-HIGHEST")
    //    lexicon.ssdi.PersonLastHigh.tagText(tokenSequence, vf, "PERSON-LAST-HIGH")
    //    lexicon.ssdi.PersonLastMedium.tagText(tokenSequence, vf, "PERSON-LAST-MEDIUM")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence, vf, "PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence, vf, "PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence, vf, "PERSON-FIRST-MEDIUM")
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence, vf, "PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence, vf, "PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence, vf, "PERSON-LAST-MEDIUM")

    /* LINE FEATURES */
    val lines = new ArrayBuffer[Seq[Token]]()
    val lineTokens = new ArrayBuffer[Token]()
    assert(tokenSequence.length > 0)
    lineTokens += tokenSequence(0)
    for (token <- tokenSequence.drop(1)) {
      if (newLine.findFirstIn(token.string).nonEmpty) {
        // some weird memory issue with ArrayBuffer here
        lines += lineTokens.toSeq
        lineTokens.clear()
      } else lineTokens += token
    }
    lines.filter(_.length > 0).foreach(line => {
      val text = line.map(_.string).mkString(" ")
      // line classifications
      if (Introduction.findFirstIn(text).nonEmpty) vf(line(0)) += "[INTRODUCTION]"
      else if (Abstract.findFirstIn(text).nonEmpty) vf(line(0)) += "[ABSTRACT]"
      else if (WeakerAbstract.findFirstIn(text).nonEmpty) vf(line.last) += "[ABSTRACT]"
      else if (Keywords.findFirstIn(text).nonEmpty) line.foreach(token => vf(token) += "lineContainsKeywords")
      // address features
      if (DocumentFeatures.containsCityStateZip(text)) line.foreach(token => vf(token) += "lineContainsCityStateZip")
      // line length features
      if (text.length < 3) vf(line.head) += "veryShortLine"
      else if (text.length < 8) vf(line.head) += "shortLine"
      if (line.length < 5) vf(line.head) += "lineLength=" + line.length
      else vf(line.head) += "lineLength>5"
    })

    //    for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))
    // TODO how to map separate offsets and conjunctions to current offsetConjunctions?
    // original code: // conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = List((0,0), (-1,0), (0,1)))
    //    val offsets = List(1, -1, 2, -2, 3, -3, 0)
    //    val conjunctions = List((0,0), (-1,0), (0,1))
    for (section <- doc.sections) addNeighboringFeatureConjunctions(section.tokens, vf, List(0,0), List(-1,0), List(0,1))

    tokenSequence.foreach(t => vf(t) += "[always-on]")

  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq
    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    //    val trainLines = trainDocs.flatMap(doc => getLines(doc).filter(_.length > 0))

    // TODO seems like an arbitrary way to make examples (but the header doesn't really have sentences, except in the abstract)
    //    val examples = scala.util.Random.shuffle(trainLines).map(line => new model.ChainLikelihoodExample(line.map(token => token.attr[LabeledBilouHeaderTag]))).toSeq
    val examples = trainDocs.flatMap(_.sections.filter(_.length > 1).map(s => new model.ChainLikelihoodExample(s.tokens.map(_.attr[LabeledBilouHeaderTag])))).toSeq

    val optimizer = new optimize.AdaGradRDA(rate=lr, l1=l1/examples.length, l2=l2/examples.length)
    def evaluate(): Unit = {
      println(s"evaluate(): processing ${trainDocs.length} train docs...")
      assert(trainDocs.length > 0)
      trainDocs.par.foreach(process)
      //      trainDocs.foreach(process)
      val sampleToks = scala.util.Random.shuffle(trainDocs(0).tokens).toSeq.take(10).map(tok =>
        s"${if (tok.attr[BilouHeaderTag].categoryValue == tok.attr[LabeledBilouHeaderTag].target.categoryValue) "" else "*"} ${if (tok.string == "\n") "nl" else tok.string} guess=${tok.attr[BilouHeaderTag].categoryValue} true=${tok.attr[LabeledBilouHeaderTag].target.categoryValue} ${tok.attr[FeatureVariable]}")
      sampleToks.foreach(println)
      println("Train accuracy "+objective.accuracy(trainLabels))
      println(new app.chain.SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, trainLabels.toIndexedSeq))
      if (!testDocs.isEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy "+objective.accuracy(testLabels))
        println(new app.chain.SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq))
      }
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    println("training...")
    optimize.Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    new app.chain.SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq).f1
  }

  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    // freeze!
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
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    is.close()
  }
}

//TODO object HeaderTagger extends BilouHeaderTagger([classpathURL] ...) etc.

class HeaderTaggerOpts extends cc.factorie.util.CmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "NER1.factorie", "STRING", "Filename for the model (saving a trained model or reading a running model.")
  val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize at all")
  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data")
  val l1 = new CmdOption("l1", 0.02, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 0.000001, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 1.0, "FLOAT", "L2 regularizer for AdaGradRDA training.")
}

object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)

    val tagger = new HeaderTagger

    assert(opts.train.wasInvoked)
    val allDocs = LoadHeaderSGML.fromFilename(opts.train.value)
    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 0.8
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 0.2

    // TODO might overlap by one?
    val trainDocs = allDocs.take((allDocs.length*trainPortionToTake).floor.toInt)
    val testDocs = allDocs.drop(trainDocs.length)
    println(s"Using ${trainDocs.length}/${allDocs.length} for training (${testDocs.length} for testing)")

    assert(opts.l1.wasInvoked && opts.l2.wasInvoked && opts.learningRate.wasInvoked)
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}")

    val result = tagger.train(trainDocs.filter(_.tokenCount > 0), testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)
    println(s"FINAL RESULT: f1 = $result")

    if (opts.saveModel.wasInvoked && opts.serialize.value){
      val fname = opts.saveModel.value
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
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-12, 1))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-12, 1))
    val lr = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-3, 10))
    val qs = new cc.factorie.util.QSubExecutor(10, "edu.umass.cs.iesl.paperheader.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, lr), qs.execute, 100, 90, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.serialize.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}

object HeaderTaggerTester {
  def main(args:Array[String]): Unit = {
    val dataPath = "/iesl/canvas/ksilvers/paperheader/data/tagged_headers.txt"
    //    val allDocs = LoadCoraHeaderSGML.fromFilename(dataPath)
    //    val docs = allDocs.take(10)
    //    val doc = allDocs(0)
    //    val tokenSeq = doc.sections.flatMap(_.tokens).toSeq
    val doc = new Document("")
    val string = "I am outside (I am inside)"
    string.split(" ").foreach(w => new Token(doc, w))
    doc.asSection.chainFreeze()
    val patternString = "(?:\"|`\\s*`).*?(?:\"|'\\s*')"
    val chunker = new RegexChunker(patternString)
    val tokenSeq = doc.sections.flatMap(_.tokens).toSeq
    chunker(tokenSeq).foreach({case (label, seq) => {
      println(s"label = $label")
      println(seq.map(_.string).mkString(" "))
    }})
    //    regexTaggers.foreach(tagger => {
    //      val labels = tagger(tokenSeq)
    //      tokenSeq.zip(labels).foreach({case (token, label) => println(s"${token.string} $label")})
    //    })

    //    val modelLoc = new java.net.URL("file:///iesl/canvas/ksilvers/paperheader/HeaderTagTest.factorie")
    //    println(s"trying to load model from: ${modelLoc.toString()}")
    //    val tagger = new HeaderTagger(url=modelLoc)

    //    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq
    //    docs.foreach(tagger.process)
    //    docs(0).sections.flatMap(_.tokens).toSeq.take(20).foreach(token => {
    //      println(s"${token.string}\t${token.attr[BilouHeaderTag].categoryValue}\t${token.attr[LabeledBilouHeaderTag].target.categoryValue}")
    //    })
    //    println(new app.chain.SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, labels.toIndexedSeq))
  }
}


