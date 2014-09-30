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

// TODO labels should maybe be on doc Sections, not Tokens ?
// TODO SegmentEvaluation ?

class HeaderTagger extends DocumentAnnotator {
  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[HeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    for (token <- document.tokens) if (token.attr[HeaderTag] eq null) token.attr += new HeaderTag(token, "O")
    val labels = document.tokens.map(_.attr[HeaderTag]).toSeq
    model.maximize(labels)(null)
    if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable] }
    //TODO document.attr.+=(new HeaderSectionBuffer ...) ?
    document
  }

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }

  class HModel extends ChainModel[HeaderTag, FeatureVariable, Token](HeaderTagDomain, FeatureDomain, l => l.token.attr[FeatureVariable], l => l.token, t => t.attr[HeaderTag])
  val model = new HModel
  val objective = new HammingTemplate[LabeledHeaderTag]


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
  
  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.strings.stringShape
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    def getFV(t:Token): FeatureVariable = { assert(t.attr[FeatureVariable] ne null); t.attr[FeatureVariable] }
    
//    val verbose = (docPrintCount <= numDocsToPrint)
//    if (verbose) docPrintCount += 1

    for (token <- doc.tokens) {
      val feats = new FeatureVariable(token)
      val word = token.string.replace(" ", "") // remove spaces
      if (word matches RexaRegex.NewLine) { println("\ttoken feats: NEWLINE"); val fv = new FeatureVariable(token); fv += "NEWLINE"; return; }
      feats += s"SIMPLIFIED=${simplify(word)}"
      feats ++= digitFeatures(word)
      feats += s"SHAPE2=${stringShape(word, 2)}"
      feats += s"SHAPE3=${stringShape(word, 1)}"

      /* TODO:
    for (m <- regexMatchers; if m(word)) this += m.name
    lexicons.foreach(_(this))
    nameLexicons.foreach(_(this))

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

//    doc.tokens.toSeq.foreach(token => {
//      val feats = new ArrayBuffer[String]()
//      val word = token.string.replace(" ", "") // remove spaces
//      if (word matches RexaRegex.NewLine) { println("\ttoken feats: NEWLINE"); val fv = new FeatureVariable(token); fv += "NEWLINE"; return; }
//      feats += s"SIMPLIFIED=${simplify(word)}"
//      feats ++= digitFeatures(word)
//      feats += s"SHAPE2=${stringShape(word, 2)}"
//      feats += s"SHAPE3=${stringShape(word, 1)}"
//
//      /* TODO:
//    for (m <- regexMatchers; if m(word)) this += m.name
//    lexicons.foreach(_(this))
//    nameLexicons.foreach(_(this))
//
//    def cmp(x:Int, y:Int) = {
//      if (x == y) "equal"
//      else if (x > y) "greater-than"
//      else "less-than"
//    }
//
//    if (hasPrev) {
//      this += "cmp(x)=" + cmp(x, prev.x)
//      this += "cmp(y)=" + cmp(y, prev.y)
//      if (font != prev.font) this += "change-in-font"
//    }
//    */
//
//
//      if (verbose && tokenPrintCount <= numTokensToPrint) {
//        tokenPrintCount += 1
//        println(s"\ttoken feats: ${feats.mkString(",")}")
//      }
//
//      val fv = new FeatureVariable(token)
//      feats.foreach(feat => fv += feat)
//    })
  

    /* LINE FEATURES
     * TODO maybe print and verify these too ?
     */
    val lines = getLines(doc)
    lines.foreach(line => {
      val text = line.map(_.string).mkString(" ")
      // line classifications
      if (text matches RexaRegex.Introduction) getFV(line(0)) += "[INTRODUCTION]"
      else if (text matches RexaRegex.Abstract) getFV(line(0)) += "[ABSTRACT]"
      else if (text matches RexaRegex.WeakerAbstract) getFV(line.last) += "[ABSTRACT]"
      else if (text matches RexaRegex.Keywords) line.foreach(token => getFV(token) += "lineContainsKeywords")

      /* TODO:
        // address features
        if (DocumentFeatures.containsCityStateZip(text)) line.foreach(_ += "lineContainsCityStateZip")
      */

      // line length features
      if (text.length < 3) getFV(line.head) += "veryShortLine"
      else if (text.length < 8) getFV(line.head) += "shortLine"
      if (line.length < 5) getFV(line.head) += "lineLength=" + line.length
      else getFV(line.head) += "lineLength>5"
    })

    //TODO
    /*
          regexTaggers.foreach(_(this))
          conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = List((0,0), (-1,0), (0,1)))
          always-on?
     */
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledHeaderTag])).toSeq
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    val trainLines = trainDocs.flatMap(doc => getLines(doc).filter(_.length > 0))
    // TODO seems like an arbitrary way to make examples (but the header doesn't really have sentences, except in the abstract)
    val examples = trainLines.map(line => new model.ChainLikelihoodExample(line.map(token => token.attr[LabeledHeaderTag]))).toSeq
    val optimizer = new optimize.AdaGradRDA(rate=lr, l1=l1/examples.length, l2=l2/examples.length)
    def evaluate(): Unit = {
      println(s"evaluate(): processing ${trainDocs.length} train docs...")
      assert(trainDocs.length > 0)
//      trainDocs.par.foreach(process)
      trainDocs.foreach(process)
      println("Train accuracy "+objective.accuracy(trainLabels))
      //      println(new app.chain.SegmentEvaluation[Labeled]("(B|U)-", "(I|L)-", BilouConllNerDomain, trainLabels.toIndexedSeq))
      if (!testDocs.isEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy "+objective.accuracy(testLabels))
        //        println(new app.chain.SegmentEvaluation[LabeledBilouConllNerTag]("(B|U)-", "(I|L)-", BilouConllNerDomain, testLabels.toIndexedSeq))
      }
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    optimize.Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    // TODO return SegmentEvaluation.f1
    0.0
  }

  //TODO serialize, deserialize
}

//TODO object HeaderTagger extends HeaderTagger([classpathURL] ...) etc.

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

    // TODO this *should* work ?
    //    val trainDocs = allDocs.take((allDocs.length*trainPortionToTake).floor.toInt)
    //    val testDocs = allDocs.drop(trainDocs.length)

    val trainPortion = (allDocs.length * trainPortionToTake).floor.toInt
    val testPortionStart = trainPortion + (allDocs.length - trainPortion)

    val trainDocs = (for (i <- 0 until trainPortion) yield allDocs(i)).toSeq
    val testDocs = (for (i <- testPortionStart until allDocs.length) yield allDocs(i)).toSeq

    assert(opts.l1.wasInvoked && opts.l2.wasInvoked && opts.learningRate.wasInvoked)
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}")
    val result = tagger.train(trainDocs, testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)
    result
  }
}

// TODO HeaderTaggerOptimizer

