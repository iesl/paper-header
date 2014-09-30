package edu.umass.cs.iesl.paperheader

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable._
import cc.factorie.app.chain._
import scala.collection.mutable.ArrayBuffer
import Features._

/**
 * Created by kate on 9/25/14.
 */

class HeaderTagger extends DocumentAnnotator {
  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BilouHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouHeaderTag])
  def process(document:Document): Document = {
    //    if (document.tokenCount == 0) return document
    if (document.tokens.toSeq.length == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    for (token <- document.tokens) {
      // prevent null ptr exceptions (not sure why this happens though)
      if (token.attr[LabeledBilouHeaderTag] eq null) token.attr += new LabeledBilouHeaderTag(token, "O")
      if (token.attr[BilouHeaderTag] eq null) token.attr += new BilouHeaderTag(token, "O")
    }
    for (section <- document.sections) {
      val labels = section.tokens.map(_.attr[BilouHeaderTag]).toSeq
      model.maximize(labels)(null)
    }
//    for (line <- getLines(document)) {
//      val labels = line.map(_.attr[BilouHeaderTag])
//      model.maximize(labels)(null)
//    }
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


  def getLines(doc:Document): ArrayBuffer[ArrayBuffer[Token]] = {
    val lines = new ArrayBuffer[ArrayBuffer[Token]]()
    val lineTokens = new ArrayBuffer[Token]()
    for (token <- doc.tokens) {
      if (token.string matches RexaRegex.NewLine) {
        lines += lineTokens
        lineTokens.clear()
      } else lineTokens += token
    }
    lines
  }


  /* globals for printing output */
  var numTokensToPrint = 20
  var tokenPrintCount = 0
  var numDocsToPrint = 5
  var docPrintCount = 0

  /*
  FIXME: "  author FeatureVariable(SIMPLIFIED=,no-digits,SHAPE2=,SHAPE3=)" -- seem to be collecting features for blank tokens ?
  TODO: conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = List((0,0), (-1,0), (0,1)))
  TODO: regexTaggers.foreach(_(this))


   */
  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.strings.stringShape
    val newLine = "(?:-NEWLINE-|\\+L\\+)".r
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.ge\ss

    for (token <- doc.tokens) {
      val feats = new FeatureVariable(token)
      token.attr += feats
      feats += "[always-on]" //TODO do we actually need this? what's it for?
      val word = token.string.replace(" ", "") // remove spaces
      //      if (word matches RexaRegex.NewLine) { println("\ttoken feats: NEWLINE"); val fv = new FeatureVariable(token); fv += "NEWLINE"; return; }
      if (newLine.findFirstIn(word).nonEmpty) feats += "NEWLINE"
      else {
        feats += s"SIMPLIFIED=${simplify(word)}"
        feats ++= digitFeatures(word)
        feats += s"SHAPE2=${stringShape(word, 2)}"
        feats += s"SHAPE3=${stringShape(word, 1)}"
        for (m <- regexMatchers; if m(word)) feats += m.name

        /* TODO:
    def cmp(x:Int, y:Int) = {
      if (x == y) "equal"
      else if (x > y) "greater-than"
      else "less-than"
    }

    if (hasPrev) {
      this += "cmp(x)=" + cmp(x, prev.x)
      this += "cmp(y)=" + cmp(y, prev.y)
      if (font != prev.font) this += "change-in-font"
    }
    */

      }
    }

    val tokenSequence = doc.tokens.toSeq
    val vf = (t:Token)=>t.attr[FeatureVariable]
    /*
    TODO:  original lexicons missing from factorie  seem to be:
    iesl/{"utexas/UNIVERSITIES", "state-abbreviations", "places", "stopwords"} and possibly:
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
    val lines = getLines(doc)
    lines.foreach(line => {
      val text = line.map(_.string).mkString(" ")
      // line classifications
      if (text matches RexaRegex.Introduction) vf(line(0)) += "[INTRODUCTION]"
      else if (text matches RexaRegex.Abstract) vf(line(0)) += "[ABSTRACT]"
      else if (text matches RexaRegex.WeakerAbstract) vf(line.last) += "[ABSTRACT]"
      else if (text matches RexaRegex.Keywords) line.foreach(token => vf(token) += "lineContainsKeywords")
      // address features
      if (DocumentFeatures.containsCityStateZip(text)) line.foreach(token => vf(token) += "lineContainsCityStateZip")
      // line length features
      if (text.length < 3) vf(line.head) += "veryShortLine"
      else if (text.length < 8) vf(line.head) += "shortLine"
      if (line.length < 5) vf(line.head) += "lineLength=" + line.length
      else vf(line.head) += "lineLength>5"
    })



  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouHeaderTag])).toSeq
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    val trainLines = trainDocs.flatMap(doc => getLines(doc).filter(_.length > 0))
    // TODO seems like an arbitrary way to make examples (but the header doesn't really have sentences, except in the abstract)
//    val examples = scala.util.Random.shuffle(trainLines).map(line => new model.ChainLikelihoodExample(line.map(token => token.attr[LabeledBilouHeaderTag]))).toSeq
    //    val examples = trainDocs.flatMap(_.sentences.filter(_.length > 1).map(sentence => new model.ChainLikelihoodExample(sentence.tokens.map(_.attr[LabeledBilouConllNerTag])))).toSeq

    val examples = trainDocs.flatMap(_.sections.filter(_.length > 1).map(s => new model.ChainLikelihoodExample(s.tokens.map(_.attr[LabeledBilouHeaderTag])))).toSeq

    val optimizer = new optimize.AdaGradRDA(rate=lr, l1=l1/examples.length, l2=l2/examples.length)
    def evaluate(): Unit = {
      println(s"evaluate(): processing ${trainDocs.length} train docs...")
      assert(trainDocs.length > 0)
//      trainDocs.par.foreach(process)
      trainDocs.foreach(process)
      val sampleToks = trainDocs(0).tokens.toSeq.take(10).map(tok =>
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
    optimize.Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    new app.chain.SegmentEvaluation[LabeledBilouHeaderTag]("(B|U)-", "(I|L)-", HeaderTagDomain, testLabels.toIndexedSeq).f1
  }

  //TODO serialize, deserialize
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

    // load docs
    assert(opts.train.wasInvoked)
    val allDocs = LoadCoraHeaderSGML.fromFilename(opts.train.value)
    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 0.8
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 0.2

    // TODO this *should* work ? might overlap by one?
    val trainDocs = allDocs.take((allDocs.length*trainPortionToTake).floor.toInt)
    val testDocs = allDocs.drop(trainDocs.length)

//    assert(trainDocs.length + testDocs.length == allDocs.length)
//    val trainDocs = allDocs.slice(0, 19)
//    val testDocs = allDocs.slice(21, 30)

    println(s"Using ${trainDocs.length}/${allDocs.length} for training (${testDocs.length} for testing)")

    assert(opts.l1.wasInvoked && opts.l2.wasInvoked && opts.learningRate.wasInvoked)
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}")
    val result = tagger.train(trainDocs, testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)

    println(s"FINAL RESULT: f1 = $result")

    result
  }
}

// TODO BilouHeaderTaggerOptimizer

