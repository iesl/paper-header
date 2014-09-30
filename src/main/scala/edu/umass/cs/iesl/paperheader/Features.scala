package edu.umass.cs.iesl.paperheader

import java.util.regex.Pattern

import cc.factorie.app.nlp._
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}

import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.util.matching.Regex


//class RegexTagger(val name:String, patternString:String, bilou:Boolean=false) {
//  val chunker = new RegexChunker(patternString)
//  def apply[T<:Token](s:Seq[T]) = {
//    for ((l,seg) <- chunker(s); if l != null) {
//      if (bilou) {
//        if (seg.length == 1) {
//          seg.head += "U-" + name
//        } else {
//          seg.slice(1, seg.length-1).foreach(_ += "I-" + name)
//          seg.head += "B-" + name
//          seg.last += "L-" + name
//        }
//      } else {
//        seg.foreach(_ += name)
//      }
//    }
//  }
//}
//
//class RegexChunker(patternString:String) {
//  val p = patternString.r
//
//  assert (!patternString.contains(" "), "must use \\s to match a space.")
//
//  private def replacerFn(m:Regex.Match): String = {
//    assert(m.groupCount == 0, "replacerFn not sure which group to tag.")
//    // must escape "$" in replacement string because it is used to reference
//    // the contents of capturing parenthesis, which this will result in illegal
//    // group references if the group doesn't exist and other nasty things.
//    " <Chunk> %s </Chunk> ".format(m.group(0).replace("$", "\\$"))
//  }
//
//  def apply[T<:Token](s:Seq[T]): Seq[(String, Seq[T])] = {
//    var seqStr = s.map(_.string.replace(" ", "<SPACE>")).mkString("    ")
//    val segments = new ListBuffer[(String, Seq[T])]
//    var tagged = p.replaceAllIn(seqStr, replacerFn _).trim.split("\\s+")
//    val tokens = s.toIterator
//    var curr = new ListBuffer[T]
//    var inside = false
//    for (w <- tagged) {
//      if (w == "<Chunk>") {
//        inside = true
//      } else if (w == "</Chunk>") {
//        inside = false
//        segments.append(("chunk", curr))
//        curr = new ListBuffer[T]
//      } else {
//        val t = tokens.next
//
//        if (t != w && t.string.replace(" ","<SPACE>") != w) {
//          return segments  // XXX: need a better fall back..
//        }
//
//        if (inside) {
//          curr += t
//        } else {
//          curr = new ListBuffer[T]
//          segments.append((null, List(t)))    // outside
//        }
//      }
//    }
//    segments
//  }
//
//}

object DocumentFeatures {
  def containsCityStateZip(x:String) = RexaRegex.CityStateZip.r.findAllIn(x).hasNext
}




// TODO maybe this proj should just depend on rexa1-metatagger so I can import ... ?

object Features {
  case class re(name:String, pattern:String) {
    val p = Pattern.compile(pattern)
    def apply(w:String) = p.matcher(w).matches
    //def contains(w:String) = p.matcher(w).find
  }
  val wordClasses = List(
    re("Year", RexaRegex.Year),
    re("Url", RexaRegex.Url),
    re("NameWithParticle", RexaRegex.NameWithParticle),
    re("TechReport", RexaRegex.TechReport),
    re("Phone", RexaRegex.Phone),
    re("PossiblePages", RexaRegex.PossiblePages),
    re("PageWord", RexaRegex.PageWord),
    re("PossibleVol", RexaRegex.PossibleVol),
    re("YearInParens", RexaRegex.YearInParens),
    re("Numeric", RexaRegex.Numeric),
    re("Digits", "[0-9]+"),
    re("DigitsAndSeparators", "[0-9]+[\\-/][0-9\\-/]*[0-9]"),
    re("DoTW", RexaRegex.DoTW),
    re("Month", RexaRegex.Month),
    re("CardinalDirection",RexaRegex.CardinalDirection),
    re("Email", RexaRegex.Email),
    re("BracketedNumber", RexaRegex.BracketedNumber),
    re("Ordinal", RexaRegex.Ordinal),
    re("OrdinalWritten", RexaRegex.OrdinalWritten),
    re("EtAl", RexaRegex.EtAl),
    re("Volume", "(?ii)Vol(?:ume|\\.?)"),
    re("No", "(?ii)N(umber|o\\.?)"),
    re("Initial", RexaRegex.Initial),
    re("Editor", RexaRegex.Editor)
  )

  def simplify(w:String): String = {
    for (m <- wordClasses; if m(w)) return m.name
    return w.replaceAll("[0-9]","#").toLowerCase
  }

  def digitFeatures(w:String) = {
    val f = new ArrayBuffer[String]
    if (w matches "[0-9]+") {
      f += "all-digits"
      f += w.length + "-digit"
      if (w.length != 4) f += "not-4-digit"
    } else if (w matches ".*[0-9].*") {
      f += "contains-digits"
    } else {
      f += "no-digits"
    }
    f
  }

  val regexMatchers = List (
    re("Punc", RexaRegex.Punc),
    re("Acronym", RexaRegex.Acronym),
    re("InParens", RexaRegex.InParens),
    re("TransitionPunc", RexaRegex.TransitionPunc),
    re("ContainsPunc", RexaRegex.ContainsPunc),
    re("Initial", RexaRegex.Initial),
    re("SingleChar", RexaRegex.SingleChar),
    re("SingleCapital", RexaRegex.SingleCapital),
    re("Capitalized", "[A-Z][a-z]+"),
    re("AllCaps", RexaRegex.AllCaps),
    re("PageWord", RexaRegex.PageWord),
    re("USZIP", "[0-9][0-9][0-9][0-9][0-9](?:-[0-9][0-9][0-9][0-9])?"),
    re("USPHONEAREACODE", "\\([0-9][0-9][0-9]\\)"),
    re("PHONEINDICATOR", "(?i)(?:fax|phone|tel|telephone|ph:)[\\.:]?"),
    re("USPHONE", "(?:\\([0-9][0-9][0-9]\\)-?)?[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]"),
    re("PHONEORZIP", "[0-9]+-[0-9]+"),
    re("TwoCaps", "[A-Z][A-Z]"),
    re("ThreeOrMoreCaps", "[A-Z][A-Z][A-Z]+"),
    re("ThreeOrMoreCapsAndSomeNumbers", "[A-Z][A-Z][A-Z]+[\\-\\s]*[0-9]")
  )

//  val regexTaggers = List (
//    // TODO: doesn't catch "month day, year" only "month year" or "month day" or 25-30 April 1992
//    new RegexTagger("DateTagger",
//      ("[(]*\\s*"
//        + RexaRegex.Month + "\\s*"
//        + "[(]*\\s*"                   /* maybe paren*/
//        + "(?:[0-9][0-9]?[\\s,]+)?"    /* maybe two-digit number */
//        + "[0-9][0-9][0-9][0-9]"       /* maybe four-digit year */
//        + "\\s*[)]{0,1}"               /* maybe paren */
//        + "(?:\\s*[,\\.])?"            /* include bits of punc at the end */ )),
//
//    new RegexTagger("PageTagger",
//      "[(]?\\s*\\b" + RexaRegex.PageWord + "\\s*[0-9][0-9\\s\\-]*[)\\.\\,\\s]*"),
//
//    new RegexTagger("VolumeTagger",
//      ("(?ii)vol(?:ume)?\\s*\\.?\\s*[0-9]+\\s*"
//        + "(?:\\s*,)?"
//        + "\\s*(?:nos?\\s*\\.?\\s*[0-9]+(?:-[0-9])?){0,1}"
//        + "(?:\\s*,)?")),
//
//    new RegexTagger("InQuotesTagger",
//      "(?:\"|`\\s*`).*?(?:\"|'\\s*')",
//      bilou=true),
//
//    // TODO: Do not tag parens preceded by a number, e.g. volumes "3(1)"
//    // TODO: parentheticals which are not years
//    new RegexTagger("SegmentInParensTagger",
//      ("\\b\\("
//        + "[^()]+?\\s+[^()]+"  /* must contain a space */
//        + "\\)\\b"),
//      bilou=true)
//  )


}

object RexaRegex {
  val YearInParens = "\\(\\s*[0-9][0-9][0-9][0-9][a-z]?\\s*\\)"      // e.g. "(1994)"
  val PossibleVol = "[0-9][0-9]?\\s*\\(\\s*[0-9]+\\s*\\)(?:\\s*:)?"  // e.g. "7(4)"
  val PossiblePages = "[0-9]+\\s*-[\\s\\-]*[0-9]+"                   // e.g. "10 - 25" possible page numbers
  val PageWord = "(?:\\b(?:(?:(?:pp|[Pp]gs?|p)(?:\\s*\\.|\\b))|[Pp]ages?))"
  val MonthStrict = "(?:January|February|March|April|May|June|July|August|September|October|November|December)"
  val Month = ("(?:(?ii)"
    + "\\b(?:Jan(?:uary)?|Febr?(?:uary)?|Mar(?:ch)?|Apr(?:il)?"
    + "|May|June?|July?|Aug(?:ust)?|Sept?(?:ember)?|Oct(?:ober)?|Nov(?:ember)?"
    + "|Dec(?:ember)?)(?:\\s*[.,/\\-]+)?\\s*"
    + ")+")
  val Editor = "(?:(?:\\(\\s*)?(?:[Ee]ditors?|[Ee]ds?\\.)(:?\\s*\\))?|\\(\\s*eds?\\s*\\)?|edited\\s*by)"
  val BracketedNumber = "\\[\\s*[0-9]+[a-z]*\\s*\\]"      // bracketed number and possibly a letter, e.g. "[15a]"
  val Url = ("(?:(?:(?:https?|ftp|ssh)://.*|(?:www|ftp)\\.\\S+|[^()@]*\\.edu\\S*)"
      + "|" + "\\((?:(?:https?|ftp|ssh)://.*|(?:www|ftp)\\.\\S+|[^()@]*\\.edu\\S*)\\))")
  val Email = "\\S+@\\S+"
  val Year = "(19|20)[0-9][0-9]"
  val Numeric = "[0-9][0-9,]+\\.?[0-9]*\\b"
  val DoTW = "(?:Mon|Tues?|Wed(?:nes)?|Thurs?|Fri|Satu?r?|Sun)(?:day)"
  val CardinalDirection = "(?:(?ii)\\b(?:north|south|east|west)\\s*?(?:east|west|))\\b"
  val Ordinal = "[0-9]+[\\-\\s]*(?:st|nd|rd|th)\\b"
  val OrdinalWritten = ("(?:(?ii)"
    + "first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth"
    + "|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth"
    + "|seventeenth|eighteenth|nineteenth"
    + "|twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth"
    + "|eightieth|ninetieth|twentieth"
    + "|hundredth|thousandth|millionth|billionth"
    + ")")

  val RefMarker = "^(?:\\[.*?\\]|[0-9]+\\s*\\.?)"

  val _NameParticle = "(?:(?ii)de|da|della|mc|del|van|van\\s*der|von|de\\s+la|la|di|van|der|O\\s*'\\s*)"
  val NameParticle = "\\b" + _NameParticle + "\\b"
  val NameWithParticle = "\\b" + _NameParticle + "\\s*[A-Z][a-zA-Z]+"

  val EtAl = "\\bet\\s*\\.?\\s*al\\b\\s*\\."
  val Phone = "\\(?[0-9][0-9][0-9][\\s\\-\\)]+[0-9][0-9][0-9][\\s\\-]+[0-9][0-9][0-9][0-9]\\b"
  val Alpha = "[A-ZÁÉÍÓÚÀÈÌÒÙÇÑÏÜa-zàèìòùáéíóúçñïü]"
  val NewLine = "(?:-NEWLINE-|\\+L\\+)"
  val Punc = "[,\\.;:?!()]"
  val Caps = "[A-ZÁÉÍÓÚÀÈÌÒÙÇÑÏÜ]"
  val CapitalizedAlphaWord = Caps + Alpha + "+"
  val Acronym = "[A-Z][A-Z\\.]*\\.[A-Z\\.]*"
  val InParens = "\\(.*\\)"

  val TransitionPunc = "[\\.\"',]"
  val ContainsPunc = ".*[\\.\"',].*"

  val Initial = "(?:" + Caps + "\\." + "\\s*)+"
  val SingleChar = Alpha
  val SingleCapital = "[A-Z]"
  val AllCaps = Caps + "+"

  val TechReport = "[A-Z\\-/]+[/-]+[0-9\\-/]+"   // not exactly a techreport .. grabs "UAI-09" "AAAI-2003"

  val References = (".*?(References?|REFERENCES?|Bibliography|BIBLIOGRAPHY"
    + "|References?\\s+and\\s+Notes?|References?\\s+Cited"
    + "|L(?i:ITERATURE CITED)"
    + "|REFERENCES?\\s+CITED|REFERENCES?\\s+AND\\s+NOTES?):?\\s*$")
  val Introduction = "[^A-Za-z]*I(?i:ntroduction)"
  val Abstract = "[^A-Za-z]*A(?i:bstract)"
  val Acknowledgments = "(?:(?ii)[^A-Za-z]*(Acknowledge?ments?))"
  val CityStateZip = "\\b(?:[A-Z][a-z]+\\s*)+, [A-Z][A-Z] [0-9][0-9][0-9][0-9][0-9](?:-[0-9]+)?"
  val WeakerAbstract = ".*\\bA(?:(?ii)bstract)\\s*:?$"
  val Keywords = "^\\s*Keywords\\s*:.*"

  // words which can appear in a name segment such as editors or authors
  val validLookingButNotValidNameWords = "\\b(?:(?ii)in|thesis|the)\\b"
  val ValidNameWord = List(
    Initial,
    SingleCapital,
    CapitalizedAlphaWord,
    NameWithParticle,
    NameParticle,
    EtAl,
    ",",
    "-+",
    "and|&"
  ).mkString("|")
}
