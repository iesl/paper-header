package edu.umass.cs.iesl.paperheader.tagger

import scala.util.matching._
import cc.factorie.app.nlp._
import scala.collection.mutable.ListBuffer
/**
 * Created by kate on 1/29/15.
 */
object Features {
  def apply(token: Token): Seq[String] = {
    val features = new ListBuffer[String]()
    features ++= Seq(
      Features.wordformFeature(token),
      Features.lemmaFeature(token),
      Features.puncFeature(token),
      Features.shapeFeature(token),
      Features.containsDigitsFeature(token)
    ).filter(_.length > 0)
    patterns.foreach({ case(label, regexes) =>
      if (regexes.count(r => r.findAllIn(token.string).nonEmpty) > 0) features += "MATCH-"+label
    })
    val cf = clusterFeatures(token)
    if (cf.length > 0) features ++= cf
    features.toSeq
  }

  def lemma(token: Token): String = cc.factorie.app.strings.simplifyDigits(token.string).toLowerCase()
  def wordformFeature(token: Token): String = s"W=${token.string}"
  def lemmaFeature(token: Token): String = s"L=${lemma(token)}"
  def puncFeature(token: Token): String = if (token.isPunctuation) "PUNC" else ""
  def shapeFeature(token: Token): String = s"SHAPE=${cc.factorie.app.strings.stringShape(token.string, 2)}"
  def containsDigitsFeature(token: Token): String = if ("\\d+".r.findAllIn(token.string).nonEmpty)"HASDIGITS" else ""
  val clusters = cc.factorie.util.JavaHashMap[String, String]()
  def prefix(prefixSize: Int, cluster: String): String = {
    if (cluster.size > prefixSize) cluster.substring(0, prefixSize) else cluster
  }
  def clusterFeatures(token: Token): Seq[String] = {
    if (clusters.size > 0 && clusters.contains(token.string)) {
      Seq(
        "CLUS="+prefix(4, clusters(token.string)),
        "CLUS="+prefix(6, clusters(token.string)),
        "CLUS="+prefix(10, clusters(token.string)),
        "CLUS="+prefix(20, clusters(token.string))
      )
    } else Seq()
  }



  val patterns = new scala.collection.mutable.HashMap[String, List[Regex]]()
  patterns("URL") = List(
    "https?://[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-]".r,
    "(?:(?:www\\.(?:[^ \t\n\f\r\"<>|.!?(){},]+\\.)+[a-zA-Z]{2,4})|(?:(?:[^ \t\n\f\r\"`'<>|.!?(){},-_$]+\\.)+(?:com|org|net|edu|gov|cc|info|uk|de|fr|ca)))(?:/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-])?".r,
    "[A-Z]*[a-z0-9]+\\.(?:com|org|net|edu|gov|co\\.uk|ac\\.uk|de|fr|ca)".r
  )
  patterns("EMAIL") = List("(?:mailto:)?\\w+[-\\+\\.'\\w]*@(?:\\w+[-\\.\\+\\w]*\\.)*\\w+".r)
  patterns("PHONE") = List(
    "(?:\\+?1[-\\. \u00A0]?)?(?:\\(?:[0-9]{3}\\)[ \u00A0]?|[0-9]{3}[- \u00A0\\.])[0-9]{3}[\\- \u00A0\\.][0-9]{4}".r,
    "(?:\\+33)?(?:\\s[012345][-\\. ])?[0-9](?:[-\\. ][0-9]{2}){3}".r
  )
  patterns("DATE") = List("(?:(?:(?:(?:19|20)?[0-9]{2}[\\-/][0-3]?[0-9][\\-/][0-3]?[0-9])|(?:[0-3]?[0-9][\\-/][0-3]?[0-9][\\-/](?:19|20)?[0-9]{2}))(?![0-9]))".r)
  patterns("MONTH") = List("Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec".r)
  patterns("DAY") = List("Mon|Tue|Tues|Wed|Thu|Thurs|Fri".r)
  patterns("ZIP") = List("\\d{5}([-]\\d{4})?".r)
}
