package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.{TokenSpanBuffer, TokenSpan, Section, Token}
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashSet, HashMap}

/**
 * Created by kate on 9/29/14.
 */


class FormatInfo(val token:Token, val xpos:Int, val ypos:Int, val fontsize:Int) {
  override def toString(): String = s"FormatInfo(${token.string} x=$xpos y=$ypos fs=$fontsize)"
}

/** stores formatting info about a group of Tokens in a Document (e.g. font size, x/y coordinates) **/
class Line(val tokens:Seq[Token], val ypos:Int, prev:Line=null) {
  def document:Document = tokens.head.document
  def start:Int = tokens.head.stringStart
  def end:Int = tokens.last.stringEnd
  def string:String = tokens.map(_.string).mkString(" ")
  def xStart: Int = tokens.head.attr[FormatInfo].xpos
  def getXVals: Seq[Int] = { tokens.map(_.attr[FormatInfo].xpos) }
  def getFontSizes: Seq[Int] = { tokens.map(_.attr[FormatInfo].fontsize) }
  //TODO probably use Option here
  def getPrevLine: Line = { if (hasPrev) prev else null}
  def hasPrev: Boolean = prev == null
  override def toString(): String = s"<Line@y=$ypos with ${tokens.length} tokens: ${tokens.map(_.string).mkString(" ")}>"
}

/** mutable collection of text blocks **/
class LineBuffer(doc:Document) {
  val blocks = new ListBuffer[Line]()
  def length = blocks.length
  def apply(i:Int): Line = if (i >= 0 && i < blocks.length) blocks(i) else throw new Error("array index out of bounds")
  def +=(line:Line): Unit = if (line.tokens.length > 0) blocks += line
}


//this should really be "IOB"
trait BIO {
  def baseDomain: CategoricalDomain[String]
  def bioSuffixIntValue(bioIntValue: Int): Int = { if (bioIntValue == 0) 0 else ((bioIntValue - 1)/3)+1 }
  def bilouTags: List[String] = "O" :: baseDomain.categories.toList.map(category => List("B-", "I-").map(_ + category)).flatten
}

trait BILOU {
  def baseDomain: CategoricalDomain[String]
  def bilouSuffixIntValue(bilouIntValue: Int): Int = { if (bilouIntValue == 0) 0 else ((bilouIntValue - 1)/4)+1 }
  def bilouTags: List[String] = "O" :: baseDomain.categories.toList.map(category => List("B-", "I-", "L-", "U-").map(_ + category)).flatten
}

//    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
//    "pubnum", "date", "note", "intro", "address", "title", "phone", "institution"
// TODO what to do with "tech", "thesis", "note"?
object BaseHeaderTagDomain extends CategoricalDomain[String] {
  this ++= Vector(
    //    "O",
    "author", //
    "institution", //
    "title", //
    //    "tech",
    //    "thesis",
    "note",
    "keyword", //
    "date", //
    "email", //
    "address", //
    "abstract" //
  )
  freeze()
}

/** BaseHeaderTagDomain categories with BILOU expansion **/
object HeaderTagDomain extends CategoricalDomain[String] with BILOU {
  def baseDomain = BaseHeaderTagDomain
  this ++= this.bilouTags.toVector
  freeze()
  def spanList(section: Section): HeaderTagSpanBuffer = {
    val boundaries = bilouBoundaries(section.tokens.map(_.attr[BilouHeaderTag].categoryValue))
    new HeaderTagSpanBuffer ++= boundaries.map(b => new HeaderTagSpan(section, b._1, b._2, b._3))
  }
}

//this should really be "IobHeaderTagDomain"
object BioHeaderTagDomain extends CategoricalDomain[String] with BIO {
  def baseDomain = BaseHeaderTagDomain
  this ++= this.bilouTags.toVector
  freeze()
  def spanList(section: Section): HeaderTagSpanBuffer = {
    val boundaries = iobBoundaries(section.tokens.map(_.attr[BioHeaderTag].categoryValue))
    new HeaderTagSpanBuffer ++= boundaries.map(b => new HeaderTagSpan(section, b._1, b._2, b._3))
  }
}

abstract class AbstractHeaderTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def baseCategoryValue: String = if (categoryValue.length > 1 && categoryValue(1) == '-') categoryValue.substring(2) else categoryValue
}
abstract class AbstractHeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory)
abstract class AbstractHeaderTagSpan(section:Section, start:Int, length:Int) extends TokenSpan(section, start, length) {
  def label: AbstractHeaderTagSpanLabel
}
class HeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory) { def domain = BaseHeaderTagDomain }
class LabeledHeaderTag(token:Token, initialCategory:String) extends HeaderTag(token, initialCategory) with CategoricalLabeling[String]
class HeaderTagSpan(section:Section, start:Int, length:Int, category:String) extends AbstractHeaderTagSpan(section, start, length) {
  val label = new HeaderTagSpanLabel(this, category)
}
class HeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends AbstractHeaderTagSpanLabel(span, initialCategory){
  def domain = BaseHeaderTagDomain
}
class HeaderTagSpanBuffer extends TokenSpanBuffer[HeaderTagSpan]

class BilouHeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory){ def domain = HeaderTagDomain }
class LabeledBilouHeaderTag(token:Token, initialCategory:String) extends BilouHeaderTag(token, initialCategory) with CategoricalLabeling[String]

class BioHeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory){ def domain = BioHeaderTagDomain }
class LabeledBioHeaderTag(token:Token, initialCategory:String) extends BioHeaderTag(token, initialCategory) with CategoricalLabeling[String]



