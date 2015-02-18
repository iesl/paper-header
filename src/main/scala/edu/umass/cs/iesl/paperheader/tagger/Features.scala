package edu.umass.cs.iesl.paperheader.tagger

import scala.util.matching._
import cc.factorie.app.nlp._
import scala.collection.mutable.ListBuffer
/**
 * Created by kate on 1/29/15.
 */

object TokenFeatures {
  def apply(token: Token): Seq[String] = {
    val features = new ListBuffer[String]()
    features ++= Seq(
      wordformFeature(token),
      lemmaFeature(token),
      puncFeature(token),
      shapeFeature(token),
      containsDigitsFeature(token)
    ).filter(_.length > 0)
    patterns.foreach({ case(label, regexes) =>
      if (regexes.count(r => r.findAllIn(token.string).nonEmpty) > 0) features += "MATCH-"+label
    })
    features ++= miscOtherTokenFeatures(token)
    val cf = clusterFeatures(token)
    if (cf.length > 0) features ++= cf
//    features ++= formattingFeatures(token)
    features.toSeq
  }
  
  def formattingFeatures(token: Token): Seq[String] = {
    val format = token.attr[FormatInfo]
    Seq(
      s"YPOS=${format.ypos}",
      s"XPOS=${format.xpos}",
      s"FS=${format.fontsize}",
      s"NYPOS=${format.ypos/FormatData.maxY}",
      s"NXPOS=${format.xpos/FormatData.maxX}"
    )
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
    val features = new ListBuffer[String]()
    val word = token.string
    val lower = word.toLowerCase
    val replace = lower.replaceAll("\\.|,|\\)", "")
    if (word.matches(Capitalized)) features += "CAPITALIZED"
    if (word.matches(AllCaps)) features += "ALLCAPS"
    if (word.matches(Numeric)) features += "NUMERIC"
    if (word.matches(ParenNumeric)) features += "PARENNUMERIC"
//    if (word.matches(Punctuation)) features += "PUNCTUATION"
    if (ContainsDigit.findFirstMatchIn(word) != None) features += "CONTAINSDIGIT"
    if (word.contains(".")) features += "CONTAINSDOTS"
    if (word.contains("-")) features += "CONTAINSDASH"
    if (word.matches("[0-9]+\\-[0-9]+")) features += "POSSIBLEPAGES"
    if (word.matches("[A-Z]")) features += "CAPLETTER"
    if (word.matches("[a-zA-Z]")) features += "SINGLECHAR"
    if (word.matches("[A-Z]\\.")) features += "LONLEYINITIAL"
//    if (word.matches(email)) features += "EMAIL"
//    if (word.matches(url)) features += "URL"
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
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) features += "BEFORE1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) features += "AFTER1900"
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
  def nerFeature(token: Token): String = if (token.nerTag != null) token.nerTag.categoryValue else ""
  def nerContextFeatures(token: Token): Seq[String] = {
    val prev = token.prevWindow(3).zipWithIndex.map(t => if (t._1.nerTag != null) s"NER@-${t._2}=${t._1.nerTag.categoryValue}" else "")
    val next = token.nextWindow(3).zipWithIndex.map(t => if (t._1.nerTag != null) s"NER@${t._2}=${t._1.nerTag.categoryValue}" else "")
    (prev ++ next).filter(_.length > 0)
  }
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

object FormatData {
  var maxY: Int = 0
  var maxX: Int = 0
  var minY: Int = 0
  var minX: Int = 0
  def getDims(doc: Document): (Int, Int, Int, Int) = {
    var Array(maxy, maxx, miny, minx) = Array(0, 0, 0, 0)
    doc.sections.flatMap(_.tokens).foreach(t => {
      val format = t.attr[FormatInfo]
      if (format.ypos > maxy) maxy = format.ypos
      if (format.ypos < miny) miny = format.ypos
      if (format.xpos > maxx) maxx = format.xpos
      if (format.xpos < minx) minx = format.xpos
    })
    (maxy, maxx, miny, minx)
  }
  def calculateMaxDims(docs: Seq[Document]): Unit = {
    docs.foreach(doc => {
      val (maxy, maxx, miny, minx) = getDims(doc)
      if (maxy > maxY) maxY = maxy
      if (miny < minY) minY = miny
      if (maxx > maxX) maxX = maxx
      if (minx < minX) minX = minx
    })
  }
  def getQuadrant(doc: Document, token: Token): Seq[String] = {
    /* grid quadrants */
    val (topy, leftx, bottomy, rightx) = getDims(doc)
    val yrange = topy - bottomy
    val medy = (yrange/2.0).floor.toInt
    val topQ = topy - medy
    val bottomQ = medy - bottomy
    val xrange = leftx - rightx
    val medx = (xrange/2.0).floor.toInt
    val leftQx = leftx - medx
    val rightQx = medx - rightx
    val format = token.attr[FormatInfo]
    //yq=0 means "topmost"; xq = 0 means "leftmost"
    val yq = if (format.ypos >= topQ) 0 else if (format.ypos <= bottomQ) 2 else 1
    val xq = if (format.xpos >= leftQx) 0 else if (format.xpos <= rightQx) 2 else 1
    Seq(s"XQ=$xq", s"YQ=$yq", s"XY=$xq$yq")
  }
}
