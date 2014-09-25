//package edu.umass.cs.iesl.paperheader
//
//import cc.factorie._
//import cc.factorie.app.tokenseq.labeled.{SegmentEvaluation}
//
//import scala.io.{Source}
//import scala.collection.mutable.{ArrayBuffer}
//
//import utils._
//import Features._
//import FactorieExtras._
//import RexaRegex._
//
//
//object OldHeaders {
//
//  object LabelDomain extends StringDomain
//  object FeatureDomain extends StringDomain
//
//  class Token(w:String, l:String, val x:Int, val y:Int, val font:String)
//    extends cc.factorie.app.tokenseq.labeled.Token[Sequence,Label,Token](w) {
//
//    def domain = FeatureDomain
//    override def skipNonCategories = !domain.growing
//    val label = new Label(l, this)
//    override val word = w.replace(" ", "")     // remove spaces
//
//    val isNewLine = word matches NewLine
//
//    def tokenFeatures {
//      if (isNewLine) { this += "[NEWLINE]"; return }
//      this += "SIMPLIFIED=" + simplify(word)
//      //this += "WORD=" + word
//      //this += "WORDLOWER=" + word.toLowerCase
//      this ++= digitFeatures(word)
//      this += "SHAPE2=" + wordShape(2)
//      this += "SHAPE1=" + wordShape(1)
//      for (m <- regexMatchers; if m(word)) this += m.name
//      lexicons.foreach(_(this))
//      nameLexicons.foreach(_(this))
//
//      def cmp(x:Int, y:Int) = {
//        if (x == y) "equal"
//        else if (x > y) "greater-than"
//        else "less-than"
//      }
//
//      if (hasPrev) {
//        this += "cmp(x)=" + cmp(x, prev.x)
//        this += "cmp(y)=" + cmp(y, prev.y)
//        if (font != prev.font) this += "change-in-font"
//      }
//    }
//
//    override def toString = "Token(\"%s\", %s)" format (word, label)
//  }
//
//  class Label(tag:String, token:Token) extends cc.factorie.app.tokenseq.labeled.Label[Sequence,Token,Label](tag, token) {
//    def domain = LabelDomain
//  }
//
//  class Sequence extends cc.factorie.app.tokenseq.labeled.TokenSeq[Token,Label,Sequence] {
//
//    override def toString = entities.map({ case (l,t) => "<%s> %s </%s>" format(l, escapeXML(t.map(_.word.replace("+L+", "\n")).mkString(" ")), l) }).mkString(" ")
//
//    lazy val lines = {
//      val lines = new ArrayBuffer[ArrayBuffer[Token]]
//      lines += new ArrayBuffer[Token]
//      for (tk <- this) {
//        if (tk.isNewLine) { lines += new ArrayBuffer[Token] }
//        else lines.last += tk
//      }
//      lines.filter(_.size > 0)
//    }
//
//    def lineFeatures {
//      import RexaRegex._
//      for (line <- lines) {
//        val text = line.map(_.word).mkString(" ")
//
//        // line classifications
//        if (text matches Introduction) line(0) += "[INTRODUCTION]"
//        else if (text matches Abstract) line(0) += "[ABSTRACT]"
//        else if (text matches WeakerAbstract) line.last += "[ABSTRACT]"
//        else if (text matches Keywords) line.foreach(_ += "lineContainsKeywords")
//
//        // address features
//        if (DocumentFeatures.containsCityStateZip(text)) line.foreach(_ += "lineContainsCityStateZip")
//
//        // line length features
//        if (text.length < 3) line.head += "veryShortLine"
//        else if (text.length < 8) line.head += "shortLine"
//        if (line.length < 5) line.head += "lineLength=" + line.length
//        else line.head += "lineLength>5"
//
//      }
//    }
//
//    def featureExtraction = {
//      this.foreach(_.tokenFeatures)
//      regexTaggers.foreach(_(this))
//      lineFeatures
//      conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = List((0,0), (-1,0), (0,1)))
//      //conjunctions(this, offsets = List(1, -1, 2, -2, 3, -3, 0), conjunctions = null)
//      for (t <- this) { t += "[always-on]" }  // add always on feature here so that we don't generate conjunctions with it.
//      this
//    }
//
//  }
//
//  class Tagger extends MyStructuredPerceptron[Label] {
//    val featureDomain = FeatureDomain
//    val labelDomain = LabelDomain
//
//    val secondOrderTemplate = new TemplateWithDotStatistics2[Label, Label] {
//      override def statisticsDomains = List(featureDomain, labelDomain)
//      override def unroll1(label:Label) = if (label.hasPrev && label.prev.hasPrev) Factor(label, label.prev.prev) else Nil
//      override def unroll2(line:Label) = Nil
//    }
//
//    val transitionTemplate = new TemplateWithDotStatistics2[Label, Label] {
//      override def statisticsDomains = List(featureDomain, labelDomain)
//      override def unroll1(label:Label) = if (label.hasPrev) Factor(label, label.prev) else Nil
//      override def unroll2(line:Label) = Nil
//    }
//
//    //    def sparsifySetttings(labels:Seq[Label]) {
//    //      transitionTemplate.sparsifySettingsFor(labels)
//    //      secondOrderTemplate.sparsifySettingsFor(labels)
//    //    }
//
//    val model = new TemplateModel (
//
//      // unary factor on first token
//      //      new TemplateWithDotStatistics1[Label] {
//      //        override def statisticsDomains = List(featureDomain, labelDomain)
//      //        override def unroll1(label:Label) = if (!label.hasPrev) Factor(label) else Nil
//      //      },
//
//      // unary factor on last token
//      //      new TemplateWithDotStatistics1[Label] {
//      //        override def statisticsDomains = List(featureDomain, labelDomain)
//      //        override def unroll1(label:Label) = if (!label.hasNext) Factor(label) else Nil
//      //      },
//
//      // LabelToken
//      new TemplateWithDotStatistics2[Label, Token] {
//        override def statisticsDomains = List(featureDomain, labelDomain)
//        override def unroll1(label:Label) = Factor(label, label.token)
//        override def unroll2(line:Token) = throw new Error("Token should not change.")
//      },
//
//      // idea: can add a factor from first to last token on a line
//      // this might encourage some sort of consistency or possibly
//      // capture interesting things which appear on the same line
//
//      // second-order transition
//      secondOrderTemplate,
//
//      // LabelLabel
//      transitionTemplate
//    )
//    val bppredictor = new BPInferencer[Label](model)
//    def predict(vs:Seq[Label]): Unit = bppredictor.infer(vs, 2)
//  }
//
//
//
//  def featureExtractionPipeline(data:Seq[Sequence], msg:String="Feature Extraction") = {
//    iterview(data, msg=msg).foreach(_.featureExtraction)
//  }
//
//
//  // load Cora headers dataset
//  object CoraHeaders {
//    def newSegment(tag:String, seg:scala.Iterator[String]) = seg.map(new Token(_, tag, x=0, y=0, font="?")).toList
//    def sequenceFromSGML(line:String) = loadSGML(line, () => new Sequence, newSegment)
//    def fromSGML(src:String, linegrouper:String="\\n") =
//      (for (line <- Source.fromFile(src).mkString.split(linegrouper))
//      yield sequenceFromSGML(line)).filter(_.size>0)
//  }
//
//  def main(args:Array[String]) {
//    val iterations = 20
//    val modelFile = "models/headers.model"
//
//    val data = CoraHeaders.fromSGML("data/tagged_headers.txt", linegrouper="<NEW_HEADER>")   /* cora-headers dataset */
//    //val data = NewHeaders.getData
//
//    val (train, test) = split(data, 0.7)
//
//    val M = new Tagger {
//      learningRate = 0.01
//      validateEvery = 5
//
//      //override def postPass { learningRate *= 0.9; super.postPass }
//
//      override def validate {
//        println
//        evaluate("TRAIN (100-samples)", train take 100)
//        println
//        evaluate("VALIDATION", test)
//        save(modelFile)
//        println
//      }
//
//      def evaluate(msg:String, data:Seq[Sequence]) {
//        print_blue(msg)
//        val labels = data.flatMap(_.labels)
//        this(labels)
//        println("LabelEvaluation")
//        println(new MyLabelEvaluation(bgLabel="O",
//          labels=labels,
//          labelDomain=LabelDomain))
//      }
//
//    }
//
//    if (args contains "load") {
//      M.load(modelFile)
//      featureExtractionPipeline(train, "Feature Extraction (train)")
//      featureExtractionPipeline(test, "Feature Extraction (test)")
//    } else {
//      featureExtractionPipeline(train, "Feature Extraction (train)")
//      LabelDomain.freeze
//      FeatureDomain.stopGrowth
//      featureExtractionPipeline(test, "Feature Extraction (test)")
//      M.fit(train.map(_.labels), iterations)
//    }
//
//    println("====================================================================")
//    println("RESULTS")
//    println("--------------------------------------------------------------------")
//    M.evaluate("TRAIN", train)
//    M.evaluate("TEST", test)
//    println("====================================================================")
//    println("DONE!")
//
//    Dump.headersDump(train, "dump/headers-train-dump.html", "Headers: Train")
//    Dump.headersDump(test, "dump/headers-test-dump.html", "Headers: Test")
//
//  }
//
//}
//
