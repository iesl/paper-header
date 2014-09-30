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


  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.strings.stringShape
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    def getFeatVec(t:Token): FeatureVariable = {
      if (t.attr[FeatureVariable] eq null) t.attr += new FeatureVariable(t)
      t.attr[FeatureVariable]
    }
    /* TOKEN FEATURES */
    for (token <- doc.tokens) {
      val feats = new FeatureVariable(token)
      token.attr += feats
      val word = token.string.replace(" ", "") // remove spaces
      if (word matches RexaRegex.NewLine) { feats += "NEWLINE"; return; }
      feats += s"SIMPLIFIED=${simplify(word)}"
      feats ++= digitFeatures(word)
      feats += s"SHAPE2=${stringShape(word, 2)}"
      feats += s"SHAPE3=${stringShape(word, 1)}"

      //TODO :
      /*

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

    /* LINE FEATURES */
    val lines = getLines(doc)
    for (line <- lines) {
      val text = line.map(_.string).mkString(" ")
      // line classifications
      if (text matches RexaRegex.Introduction) getFeatVec(line(0)) += "[INTRODUCTION]"
      else if (text matches RexaRegex.Abstract) getFeatVec(line(0)) += "[ABSTRACT]"
      else if (text matches RexaRegex.WeakerAbstract) getFeatVec(line.last) += "[ABSTRACT]"
      else if (text matches RexaRegex.Keywords) line.foreach(token => getFeatVec(token) += "lineContainsKeywords")

      //TODO
      /*
              // address features
        if (DocumentFeatures.containsCityStateZip(text)) line.foreach(_ += "lineContainsCityStateZip")
        
        */

      // line length features
      if (text.length < 3) getFeatVec(line.head) += "veryShortLine"
      else if (text.length < 8) getFeatVec(line.head) += "shortLine"
      if (line.length < 5) getFeatVec(line.head) += "lineLength=" + line.length
      else getFeatVec(line.head) += "lineLength>5"
    }

    //TODO
    /*
          regexTaggers.foreach(_(this))
          conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = List((0,0), (-1,0), (0,1)))
          always-on?
     */
  }

  /*
    class SentenceClassifierExample(val tokens:Seq[Token], model:LinearMulticlassClassifier, lossAndGradient: optimize.OptimizableObjectives.Multiclass) extends optimize.Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
      val lemmaStrings = lemmas(tokens)
      for (index <- 0 until tokens.length) {
        val token = tokens(index)
        val posLabel = token.attr[LabeledPennPosTag]
        val featureVector = features(token, index, lemmaStrings)
        new optimize.PredictorExample(model, featureVector, posLabel.target.intValue, lossAndGradient, 1.0).accumulateValueAndGradient(value, gradient)
        if (exampleSetsToPrediction) {
          posLabel.set(model.classification(featureVector).bestLabelIndex)(null)
        }
      }
    }
  }
   */

//  class LineClassifierExample(val tokens:Seq[Token])


  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledHeaderTag])).toSeq
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    //in ConllChainNer:
    //    val examples = trainDocs.flatMap(_.sentences.filter(_.length > 1).map(sentence => new model.ChainLikelihoodExample(sentence.tokens.map(_.attr[LabeledBilouConllNerTag])))).toSeq
//    val examples = trainDocs.flatMap(_.tokens).toSeq.map(token => new model.ChainLikelihoodExample(token.attr[LabeledHeaderTag])).toSeq
    val trainTokens = trainDocs.flatMap(_.tokens).toSeq
    val examples = trainTokens.map(token => new model.ChainLikelihoodExample(token.attr[LabeledHeaderTag])).toSeq


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

    0.0
  }
}


