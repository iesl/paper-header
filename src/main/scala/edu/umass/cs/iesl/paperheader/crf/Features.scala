package edu.umass.cs.iesl.paperheader.crf

/**
 * @author Kate Silverstein 
 *         created on 3/23/15
 */

import cc.factorie.app.nlp._
import cc.factorie.app.strings._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import scala.util.matching.Regex

object Features {
  lazy val patterns: Map[String, List[Regex]] = Map(
    "URL" -> List(
      "https?://[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-]".r,
      "(?:(?:www\\.(?:[^ \t\n\f\r\"<>|.!?(){},]+\\.)+[a-zA-Z]{2,4})|(?:(?:[^ \t\n\f\r\"`'<>|.!?(){},-_$]+\\.)+(?:com|org|net|edu|gov|cc|info|uk|de|fr|ca)))(?:/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-])?".r,
      "[A-Z]*[a-z0-9]+\\.(?:com|org|net|edu|gov|co\\.uk|ac\\.uk|de|fr|ca)".r
    ),
    "EMAIL" -> List("(?:mailto:)?\\w+[-\\+\\.'\\w]*@(?:\\w+[-\\.\\+\\w]*\\.)*\\w+".r),
    "PHONE" -> List(
      "(?:\\+?1[-\\. \u00A0]?)?(?:\\(?:[0-9]{3}\\)[ \u00A0]?|[0-9]{3}[- \u00A0\\.])[0-9]{3}[\\- \u00A0\\.][0-9]{4}".r,
      "(?:\\+33)?(?:\\s[012345][-\\. ])?[0-9](?:[-\\. ][0-9]{2}){3}".r
    ),
    "DATE" -> List("(?:(?:(?:(?:19|20)?[0-9]{2}[\\-/][0-3]?[0-9][\\-/][0-3]?[0-9])|(?:[0-3]?[0-9][\\-/][0-3]?[0-9][\\-/](?:19|20)?[0-9]{2}))(?![0-9]))".r),
    "MONTH" -> List("Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec".r),
    "DAY" -> List("Mon|Tue|Tues|Wed|Thu|Thurs|Fri".r),
    "ZIP" -> List("\\d{5}([-]\\d{4})?".r)
  )
  def lemma(t:Token): String = simplifyDigits(t.string).toLowerCase
  def extractTokenFeatures(token: Token): Seq[String] = {
    val feats = new ArrayBuffer[String]()
    feats += s"W=${lemma(token)}"
    if (token.isPunctuation) feats += "PUNCT"
    if (token.isCapitalized) feats += "CAP"
    feats += s"SHAPE=${stringShape(token.string, 2)}"
    if ("\\d+".r.findAllIn(token.string).nonEmpty) feats += "HASDIGITS"
    feats ++= patterns.map({ case (label, regexes) => if (regexes.count(r => r.findAllIn(token.string).nonEmpty) > 0) "MATCH-"+label else "" }).toList.filter(_.length > 0)
    feats.toSeq
  }
  def tagWithLexicons(tokenSeq: Seq[Token], vf: (Token => cc.factorie.variable.CategoricalVectorVar[String])): Unit = {
    //DATE
    BibtexDate.tagText(tokenSeq, vf, "BIBDATE")
    lexicon.iesl.Month.tagText(tokenSeq ,vf,"MONTH")
    lexicon.iesl.Day.tagText(tokenSeq ,vf,"DAY")
    //ADDRESS
    lexicon.wikipedia.Location.tagText(tokenSeq, vf, "WIKI-LOCATION")
    lexicon.iesl.Country.tagText(tokenSeq,vf, "COUNTRY")
    lexicon.iesl.City.tagText(tokenSeq,vf, "CITY")
    lexicon.iesl.USState.tagText(tokenSeq,vf, "USSTATE")
    lexicon.iesl.PlaceSuffix.tagText(tokenSeq, vf, "PLACE-SUFFIX")
    //INSTITUTION
    lexicon.wikipedia.Organization.tagText(tokenSeq, vf, "WIKI-ORG")
    Affiliation.tagText(tokenSeq, vf, "BIBAFFILIATION")
    //AUTHOR
    BibtexAuthor.tagText(tokenSeq, vf, "BIBAUTHOR")
    lexicon.iesl.PersonFirst.tagText(tokenSeq,vf,"PERSON-FIRST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSeq,vf,"PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSeq,vf,"PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSeq,vf,"PERSON-FIRST-MEDIUM")
    lexicon.iesl.PersonLast.tagText(tokenSeq,vf,"PERSON-LAST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSeq,vf,"PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastHighest.tagText(tokenSeq,vf,"PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastMedium.tagText(tokenSeq,vf,"PERSON-LAST-MEDIUM")
    lexicon.iesl.PersonHonorific.tagText(tokenSeq,vf,"PERSON-HONORIFIC")
  }
}

object BibtexAuthor extends lexicon.TriePhraseLexicon("bibtex-author") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_author_full"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}
object BibtexDate extends lexicon.TriePhraseLexicon("bibtex-date") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_date"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("could not find resource") } }
}
object Note extends lexicon.TriePhraseLexicon("note") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_note"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}
object Affiliation extends lexicon.TriePhraseLexicon("affiliation") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_affiliation"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}
