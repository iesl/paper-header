package edu.umass.cs.iesl.paperheader

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable._
import cc.factorie.app.chain._

/**
* Created by kate on 9/25/14.
*/

class HeaderTagger extends DocumentAnnotator {
  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = token.attr[HeaderField].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderField])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    //TODO stuff
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
}



//object DocHeaderExtractor {
//  def main(args:Array[String]): Unit = {
//    println("DocHeaderExtractor")
//  }
//}
