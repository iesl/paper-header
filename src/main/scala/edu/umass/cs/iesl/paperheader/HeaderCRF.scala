package edu.umass.cs.iesl.paperheader

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable._
import cc.factorie.app.chain._
import Features._

/**
 * Created by kate on 9/25/14.
 */

// TODO labels should maybe be on doc Sections, not Tokens ?

class HeaderTagger extends DocumentAnnotator {
  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = token.attr[HeaderField].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderField])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    for (token <- document.tokens) if (token.attr[HeaderField] eq null) token.attr += new HeaderField(token, "O")
    val labels = document.tokens.map(_.attr[HeaderField]).toSeq
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

  class Model extends ChainModel[HeaderField, FeatureVariable, Token](HeaderFieldDomain, FeatureDomain, l => l.token.attr[FeatureVariable], l => l.token, t => t.attr[HeaderField])
  val model = new Model
  val objective = new HammingTemplate[LabeledHeaderField]



  def addFeatures(doc:Document): Unit = {
    import cc.factorie.app.strings.stringShape
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
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


  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1)(implicit random:scala.util.Random): Double = {
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


