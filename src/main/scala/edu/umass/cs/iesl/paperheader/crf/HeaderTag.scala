package edu.umass.cs.iesl.paperheader.crf

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.{TokenSpanBuffer, TokenSpan, Section, Token}
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashSet, HashMap}

/**
 * Created by kate on 9/29/14.
 */

class HeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends NerSpanLabel(span, initialCategory) { def domain = HeaderTagDomain }
class HeaderTagSpan(section:Section, start:Int, length:Int, category:String) extends NerSpan(section, start, length) { val label = new HeaderTagSpanLabel(this, category) }
class HeaderTagSpanBuffer extends TokenSpanBuffer[HeaderTagSpan]
object HeaderTagDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "abstract",
    "address",
    "author",
    "date",
    "email",
    "institution",
    "keyword",
    "note",
    "tech",
    "thesis",
    "title"
  )
  freeze()
}

object BilouHeaderTagDomain extends CategoricalDomain[String] with BILOU {
  this ++= encodedTags(HeaderTagDomain.categories)
  freeze()
  def spanList(section: Section): HeaderTagSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouHeaderTag].categoryValue))
    new HeaderTagSpanBuffer ++= boundaries.map(b => new HeaderTagSpan(section, b._1, b._2, b._3))
  }
}
class BilouHeaderTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BilouHeaderTagDomain }
class LabeledBilouHeaderTag(token:Token, initialCategory:String) extends BilouHeaderTag(token, initialCategory) with CategoricalLabeling[String]

//this should really be "IobHeaderTagDomain"
object BioHeaderTagDomain extends CategoricalDomain[String] with BIO {
  this ++= encodedTags(HeaderTagDomain.categories)
  freeze()
  def spanList(section: Section): HeaderTagSpanBuffer = {
    val boundaries = iobBoundaries(section.tokens.map(_.attr[BioHeaderTag].categoryValue))
    new HeaderTagSpanBuffer ++= boundaries.map(b => new HeaderTagSpan(section, b._1, b._2, b._3))
  }
}
class BioHeaderTag(token:Token, initialCategory:String) extends NerTag(token, initialCategory) { def domain = BioHeaderTagDomain }
class LabeledBioHeaderTag(token:Token, initialCategory:String) extends BioHeaderTag(token, initialCategory) with CategoricalLabeling[String]

class FormatInfo(val token:Token, val xPos:Int, val yPos:Int, val fontSize:Int){
  override def toString(): String = s"FormatInfo(${token.string} x=$xPos y=$yPos fs=$fontSize)"
}

/** stores formatting info about a group of Tokens in a Document (e.g. font size, x/y coordinates) **/
@deprecated("formatting info shouldnt be used for anything for now")
class Line(val tokens:Seq[Token], val ypos:Int, prev:Line=null) { //extends TokenSpan(section, start, length){
def document:Document = tokens.head.document
  def start:Int = tokens.head.stringStart
  def end:Int = tokens.last.stringEnd
  def string:String = tokens.map(_.string).mkString(" ")
  def xStart: Int = tokens.head.attr[FormatInfo].xPos
  def getXVals: Seq[Int] = { tokens.map(_.attr[FormatInfo].xPos) }
  def getFontSizes: Seq[Int] = { tokens.map(_.attr[FormatInfo].fontSize) }
  //TODO probably use Option here
  def getPrevLine: Line = { if (hasPrev) prev else null}
  def hasPrev: Boolean = prev == null
  override def toString(): String = s"<Line@y=$ypos with ${tokens.length} tokens>"
}

/** mutable collection of TextBlocks **/
@deprecated("formatting info shouldnt be used for anything for now")
class LineBuffer(doc:Document) {
  val blocks = new ListBuffer[Line]()
  def length = blocks.length
  def apply(i:Int): Line = if (i >= 0 && i < blocks.length) blocks(i) else throw new Error("array index out of bounds")
  def +=(line:Line): Unit = blocks += line
}