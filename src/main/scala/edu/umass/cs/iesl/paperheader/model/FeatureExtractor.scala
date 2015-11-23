package edu.umass.cs.iesl.paperheader.model

import java.util.logging.Logger

import cc.factorie.app.nlp.Token
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._

/**
 * Created by kate on 11/14/15.
 */
object FeatureExtractor {

  private val log = Logger.getLogger(getClass.getName)

  def firstOrderFeatures(token: Token): Seq[String] = {
    val features = new ArrayBuffer[String]()
    features ++= Seq(
      lemmaFeature(token),
      puncFeature(token),
      shapeFeature(token),
      containsDigitsFeature(token),
      positionFeature(token)
    ).filter(_.length > 0)
    patterns.foreach { case (label, regexes) =>
      if (regexes.count(r => r.findAllIn(token.string).nonEmpty) > 0) features += label
    }
    features ++= miscOtherTokenFeatures(token)
    val cf = clusterFeatures(token)
    if (cf.nonEmpty) features ++= cf
    features.toSeq
  }

  def process(token: Token): Seq[String] = {
    val features = new ArrayBuffer[String]()
    features ++= Seq(
      lemmaFeature(token),
      puncFeature(token),
      shapeFeature(token),
      containsDigitsFeature(token)
    ).filter(_.length > 0)
    patterns.foreach { case (label, regexes) =>
      if (regexes.count(r => r.findAllIn(token.string).nonEmpty) > 0) features += "P"+label
    }
    if (token.hasPrev && token.hasNext) features ++= trigramFeats(token.prev, token, token.next)
    if (token.hasPrev) features ++= bigramFeats(token.prev, token)
    if (token.hasNext) features ++= bigramFeats(token, token.next)
    features ++= miscOtherTokenFeatures(token)
    val cf = clusterFeatures(token)
    if (cf.length > 0) features ++= cf
    features.toSeq
  }

  def bigramFeats(tok_1: Token, token: Token): Seq[String] = {
    val tri = List(tok_1, token).map(_.string).mkString("")
    val feats = new ArrayBuffer[String]()
    patterns.keySet.foreach(label => {
      val regexes = patterns(label)
      for (r <- regexes) {
        if (r.findAllIn(tri).nonEmpty) feats += "P"+label
      }
    })
    feats.toSet.toSeq
  }

  def trigramFeats(tok_1: Token, token: Token, tok1: Token): Seq[String] = {
    val tri = List(tok_1, token, tok1).map(_.string).mkString("")
    val feats = new ArrayBuffer[String]()
    patterns.keySet.foreach(label => {
      val regexes = patterns(label)
      for (r <- regexes) {
        if (r.findAllIn(tri).nonEmpty) feats += "P"+label
      }
    })
    feats.toSet.toSeq
  }

  val Capitalized = "^[A-Z].*"
  val AllCaps = "^[A-Z]*"
  val Numeric = "^[0-9]+$"
  val ParenNumeric = "^\\([0-9]+\\).?$"
  val Punctuation = "[-,\\.;:?!()]+"
  val EndPeriod = ".*\\.$"
  val EndFullColon = ".*\\:$"
  val EndComma = ".*\\,$"
  val HasOpenParen = ".*\\(.*"
  val HasClosedParen = ".*\\).*"
  val HasOpenSquare = ".*\\[.*"
  val HasClosedSquare = ".*\\].*"
  val ContainsDigit = ".*[0-9].*".r

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

  def count(string: String): (Int, Int) = {
    var digits = 0
    var alpha = 0
    for (char <- string) {
      if (char.toString.matches("[0-9]")) digits += 1
      else if (char.toString.matches("[a-zA-Z]")) alpha += 1
    }
    (digits, alpha)
  }

  def miscOtherTokenFeatures(token: Token): Seq[String] = {
    val features = new ArrayBuffer[String]()
    val word = token.string
    val lower = word.toLowerCase
    val replace = lower.replaceAll("\\.|,|\\)", "")
    if (word.matches(Capitalized)) features += "CAPITALIZED"
    if (word.matches(AllCaps)) features += "ALLCAPS"
    if (word.matches(Numeric)) features += "NUMERIC"
    if (word.matches(ParenNumeric)) features += "PARENNUMERIC"
    if (word.matches(Punctuation)) features += "PUNCTUATION"
    if (ContainsDigit.findFirstMatchIn(word) != None) features += "CONTAINSDIGIT"
    if (word.contains(".")) features += "CONTAINSDOTS"
    if (word.contains("-")) features += "CONTAINSDASH"
    if (word.matches("[0-9]+\\-[0-9]+")) features += "POSSIBLEPAGES"
    if (word.matches("[A-Z]")) features += "CAPLETTER"
    if (word.matches("[a-zA-Z]")) features += "SINGLECHAR"
    if (word.matches("[A-Z]\\.")) features += "LONLEYINITIAL"
    if (word.matches(EndComma)) features += "ENDCOMMA"
    if (word.matches(EndPeriod)) features += "ENDPERIOD"
    if (word.matches(EndFullColon)) features += "ENDFULLCOLON"
    if (word.matches(HasOpenParen)) features += "OPENPAREN"
    if (word.matches(HasClosedParen)) features += "CLOSEDPAREN"
    if (word.matches(HasOpenParen) && word.matches(HasClosedParen)) features += "OPENANDSHUT"
    if (word.matches(HasOpenSquare)) features += "OPENSQUARE"
    if (word.matches(HasClosedSquare)) features += "CLOSEDSQUARE"
    if (word.matches(".*[0-9]$")) features += "LASTNUM"
    if (word.matches(".*[A-Z]$")) features += "LASTUPPER"
    if (word.matches(".*[a-z]$")) features += "LASTLOWER"
    if (word.matches(".*\"$")) features += "ENDQUOTE"
    if (word.matches(".*\".$")) features += "QUOTEATEND"
    if (word.matches(".*;$")) features += "ENDSEMI"
    if (word.matches("^\".*")) features += "BEGINQUOTE"
    if (word.matches(".*\\\\")) features += "ENDFORWARD"
    if (word.matches("^\\\\.*")) features += "BEGINFORWARD"
    if (word.matches(".*,\"$")) features += "ENDCOMMAQUOTE"
    if (word.matches("^[\\'`].*")) features += "STARTSINGLEQUOTE"
    if (word.matches(".*'.?$")) features += "ENDSINGLEQUOTE"
    if (word.trim.toLowerCase == "and" || word.trim.toLowerCase == "&") features += "ISAND"
    if (word.matches(".*[1-9](th|nd|st).*")) features += "EVENTITERATION"
    val counts = count(word)
    features += "NUMDIGITS=" + counts._1
    features += "NUMALPHA=" + counts._2
    features += "NUMDIGITS=" + counts._1 + "ALPHS=" + counts._2
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt < 1900) features += "BEFORE1900"
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt >= 1900) features += "AFTER1900"
    if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) features += "BEFORE1900"
    if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) features += "AFTER1900"
    if (lower.startsWith("appeared") || lower.startsWith("submitted") || lower.startsWith("appear")) features += "STATUS"
    //if(token.startsSpansOfClass[SegmentSpan].nonEmpty) features += "PROBABLESEGMENT"
    //    if (docSpans.exists(span => span.tokens.head == token)) features += "PROBABLESEGMENT"
    if (lower.matches("(ed\\.|eds\\.|editor|editors).*")) features += "EDITOR"
    if (lower.matches("(proc\\.?|proceedings|trans\\.?|conf\\.?|symp\\.?|conference|symposium|workshop).*")) features += "BOOKTITLE"
    if (lower.matches("(university|dept\\.|department).*")) features += "INST"
    if (lower.matches("^p(p|ages|pps|gs)?\\.?")) features += "ISPAGES"
    if (lower.matches("(v\\.?|volume|vol\\.?).*")) features += "VOLUME"
    features.toSeq
  }

  /* surface form features */
  def lemma(token: Token): String = cc.factorie.app.strings.simplifyDigits(token.string).toLowerCase()
  def wordformFeature(token: Token): String = s"W=${token.string}"
  def lemmaFeature(token: Token): String = s"L=${lemma(token)}"
  def puncFeature(token: Token): String = if (token.isPunctuation) "PUNC" else ""
  def shapeFeature(token: Token): String = s"SHAPE=${cc.factorie.app.strings.stringShape(token.string, 2)}"
  def containsDigitsFeature(token: Token): String = if ("\\d+".r.findAllIn(token.string).nonEmpty)"HASDIGITS" else ""
  def positionFeature(token: Token): String = s"IDX=${token.position}"

  val clusters = cc.factorie.util.JavaHashMap[String, String]()
  def prefix(prefixSize: Int, cluster: String): String = {
    if (cluster.size > prefixSize) cluster.substring(0, prefixSize) else cluster
  }
  def clusterFeatures(token: Token): Seq[String] = {
    if (clusters.nonEmpty && clusters.contains(token.string)) {
      Seq(
        "CLUS="+prefix(4, clusters(token.string)),
        "CLUS="+prefix(6, clusters(token.string)),
        "CLUS="+prefix(10, clusters(token.string)),
        "CLUS="+prefix(20, clusters(token.string))
      )
    } else Seq()
  }


}
