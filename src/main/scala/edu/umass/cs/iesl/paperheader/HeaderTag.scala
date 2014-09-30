package edu.umass.cs.iesl.paperheader

import cc.factorie.app.nlp.ner.NerSpanLabel
import cc.factorie.app.nlp.{TokenSpanBuffer, TokenSpan, Section, Token}
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}

/**
 * Created by kate on 9/29/14.
 */

object HeaderTagDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
    "pubnum", "date", "note", "intro", "address", "title", "phone",
    "O" //blank/default tag
  )
  freeze()
}

/*
because:

[ERROR] /iesl/canvas/ksilvers/paperheader/src/main/scala/edu/umass/cs/iesl/paperheader/HeaderCRF.scala:138: error: type mismatch;
[INFO]  found   : edu.umass.cs.iesl.paperheader.LabeledHeaderTag
[INFO]  required: Seq[edu.umass.cs.iesl.paperheader.HeaderTag with cc.factorie.variable.LabeledMutableDiscreteVar]
[ERROR] Error occurred in an application involving default arguments.
[INFO]     val examples = trainDocs.flatMap(_.tokens).map(token => new model.ChainLikelihoodExample(token.attr[LabeledHeaderTag])).toSeq
[INFO]
 */
abstract class AbstractHeaderTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory)
abstract class AbstractHeaderSpanLabel(val span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory)
abstract class AbstractHeaderSpan(section:Section, start:Int, length:Int) extends TokenSpan(section, start, length){
  def label:AbstractHeaderSpanLabel
  override def toString = "HeaderSpan("+length+","+label.categoryValue+":"+this.phrase+")"
}


//class HeaderTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) { def domain = HeaderTagDomain }
class HeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory) { def domain = HeaderTagDomain }

class LabeledHeaderTag(token:Token, initialCategory:String) extends HeaderTag(token, initialCategory) with CategoricalLabeling[String]

class HeaderTagSpan(section:Section, start:Int, length:Int, category:String) extends AbstractHeaderSpan(section, start, length) {
  val label = new HeaderTagSpanLabel(this, category)
}

class HeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends AbstractHeaderSpanLabel(span, initialCategory){
  def domain = HeaderTagDomain
}

class HeaderTagSpanBuffer extends TokenSpanBuffer[HeaderTagSpan]


//class HeaderFieldLabel(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) { def domain = HeaderFieldDomain }

//class LabeledHeaderSection(val document:Document, val stringStart:Int, val stringEnd:Int, val label:String, val text:String) { //extends Section(document, stringStart, stringEnd) {
//  val whitespace = "\\s".r
//  def tokenize(): Seq[Token] = {
//    val words = whitespace.split(text)
//    val tokens = words.map(w => {
//      val token = new Token(document, w)
//      token.attr += new HeaderFieldLabel(token, label)
//      token
//    })
//    tokens
//  }
//  override def toString(): String = s"<LabeledHeaderSection $label>"
//}