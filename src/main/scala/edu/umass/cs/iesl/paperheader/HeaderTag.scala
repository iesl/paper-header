package edu.umass.cs.iesl.paperheader

import cc.factorie.app.nlp
import cc.factorie.app.nlp.{TokenSpanBuffer, TokenSpan, Section, Token}
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}
import scala.collection.mutable.ListBuffer

/**
 * Created by kate on 9/29/14.
 */

trait BILOU {
  def baseDomain: CategoricalDomain[String]
  def bilouSuffixIntValue(bilouIntValue: Int): Int = { if (bilouIntValue == 0) 0 else ((bilouIntValue - 1)/4)+1 }
  def bilouTags: ListBuffer[String] = {
    val categoryLabels = new ListBuffer[String]
    val prefixes = Vector("B-", "I-", "L-", "U-")
    for (category <- baseDomain.categories; prefix <- prefixes) {
      if (category.toString != "O") categoryLabels += prefix+category.toString
    }
    categoryLabels += "O"
    categoryLabels
  }
}

object BaseHeaderTagDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
    "pubnum", "date", "note", "intro", "address", "title", "phone"
  )
  freeze()
}

object HeaderTagDomain extends CategoricalDomain[String] with BILOU {
  def baseDomain = BaseHeaderTagDomain
  this ++= this.bilouTags.toVector
  freeze()
  def spanList(section: Section): HeaderTagSpanBuffer = {
    val boundaries = nlp.bilouBoundaries(section.tokens.map(_.attr[BilouHeaderTag].categoryValue))
    new HeaderTagSpanBuffer ++= boundaries.map(b => new HeaderTagSpan(section, b._1, b._2, b._3))
  }
}

/*
TODO clean this various classes up (eg don't need both AbstractHeaderTag and HeaderTag)
 */

abstract class AbstractHeaderTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def baseCategoryValue: String = if (categoryValue.length > 1 && categoryValue(1) == '-') categoryValue.substring(2) else categoryValue
}
abstract class AbstractHeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory)
abstract class AbstractHeaderTagSpan(section:Section, start:Int, length:Int) extends TokenSpan(section, start, length) {
  def label: AbstractHeaderTagSpanLabel
}

//class HeaderTag(val token:Token, initialCategory:String) extends CategoricalVariable(initialCategory) { def domain = HeaderTagDomain }
class HeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory) { def domain = BaseHeaderTagDomain }
class LabeledHeaderTag(token:Token, initialCategory:String) extends HeaderTag(token, initialCategory) with CategoricalLabeling[String]

class HeaderTagSpan(section:Section, start:Int, length:Int, category:String) extends AbstractHeaderTagSpan(section, start, length) {
  val label = new HeaderTagSpanLabel(this, category)
}
//class HeaderTagSpan(section:Section, start:Int, length:Int, category:String) extends TokenSpan(section, start, length) {
//  val label = new HeaderTagSpanLabel(this, category)
//}

class HeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends AbstractHeaderTagSpanLabel(span, initialCategory){
  def domain = BaseHeaderTagDomain
}
//class HeaderTagSpanLabel(span:TokenSpan, initialCategory:String) extends CategoricalVariable(initialCategory){
//  def domain = HeaderTagDomain
//}

class HeaderTagSpanBuffer extends TokenSpanBuffer[HeaderTagSpan]

class BilouHeaderTag(token:Token, initialCategory:String) extends AbstractHeaderTag(token, initialCategory){ def domain = HeaderTagDomain }
class LabeledBilouHeaderTag(token:Token, initialCategory:String) extends BilouHeaderTag(token, initialCategory) with CategoricalLabeling[String]


