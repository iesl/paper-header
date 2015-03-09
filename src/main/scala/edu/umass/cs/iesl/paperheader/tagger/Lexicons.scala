package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp.lexicon._
import cc.factorie.app.nlp.Token
import scala.collection.mutable.{HashMap, ArrayBuffer}

import scala.io.Source

/**
 * @author Kate Silverstein 
 *         created on 2/18/15
 */

object LexiconTagger {
  val lexiconMap = new HashMap[String, TriePhraseLexicon]()
  //authors
  lexiconMap("author1") = BibtexAuthor
  lexiconMap("author2") = BibieAuthor
  lexiconMap("person-first-highest") = iesl.PersonFirstHighest
  lexiconMap("person-first-high") = iesl.PersonFirstHigh
  lexiconMap("person-first-med") = iesl.PersonFirstMedium
  lexiconMap("person-last-highest") = iesl.PersonLastHighest
  lexiconMap("person-last-high") = iesl.PersonLastHigh
  lexiconMap("person-last-med") = iesl.PersonLastMedium
  lexiconMap("person-honorific") = iesl.PersonHonorific
  //addresses
  lexiconMap("continent") = iesl.Continents
  lexiconMap("country") = iesl.Country
  lexiconMap("city") = iesl.City
  lexiconMap("us-state") = iesl.USState
  lexiconMap("place-suffix") = iesl.PlaceSuffix
  lexiconMap("org-suffix") = iesl.OrgSuffix
  lexiconMap("place") = BibiePlace
  //dates
  lexiconMap("date") = BibtexDate
  lexiconMap("month") = iesl.Month
  lexiconMap("day") = iesl.Day
  //institution
  lexiconMap("institut") = BibieInstitution
  lexiconMap("org") = wikipedia.Organization
  //titles
  lexiconMap("title1") = BibtexTitle
  lexiconMap("title2") = BibieTitle
  
  def tagText(tokens: Seq[Token], vf: (Token => cc.factorie.variable.CategoricalVectorVar[String])): Unit = {
    for (k <- lexiconMap.keySet) lexiconMap(k).tagText(tokens, vf, k)
  }
  

  def getLexiconTags(token: Token): Seq[String] = {
    val tags = new ArrayBuffer[String]()
    val lemma = TokenFeatures.lemma(token)
    for (k <- lexiconMap.keySet) {
      if (lexiconMap(k).containsLemmatizedWord(lemma)) tags += k
    }
    tags
  }
  
  def getLexiconTags(tokens: Seq[Token]): Seq[String] = {
    val tags = new ArrayBuffer[String]()
    val lemmas = tokens.map(TokenFeatures.lemma)
    for (k <- lexiconMap.keySet) {
      if (lexiconMap(k).containsLemmatizedWords(lemmas)) tags += k
    }
    tags
  }
}


/* lexicons from "bibie" */
object BibieAuthor extends TriePhraseLexicon("bibie-author") {
  val reader = Source.fromURL(getClass.getResource("/bibie-lexicons/authors.lst"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}
object BibiePlace extends TriePhraseLexicon("bibie-place") {
  val reader = Source.fromURL(getClass.getResource("/bibie-lexicons/known_place.lst"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}
object BibieTitle extends TriePhraseLexicon("bibie-title") {
  val reader = Source.fromURL(getClass.getResource("/bibie-lexicons/known_title.lst"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}
object BibieInstitution extends TriePhraseLexicon("bibie-institution") {
  val reader = Source.fromURL(getClass.getResource("/bibie-lexicons/institution.lst"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}




/* "bibtex" lexicons */
object BibtexAuthor extends TriePhraseLexicon("bibtex-author") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_author_full"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}

object BibtexDate extends TriePhraseLexicon("bibtex-date") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_date"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("could not find resource") } }
}

object BibtexNote extends TriePhraseLexicon("bibtex-note") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_note"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}

object BibtexAffiliation extends TriePhraseLexicon("bibtex-affiliation") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_affiliation"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}

object BibtexPublication extends TriePhraseLexicon("publication") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_pubs"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}

object BibtexTitle extends TriePhraseLexicon("title") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_titles"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}