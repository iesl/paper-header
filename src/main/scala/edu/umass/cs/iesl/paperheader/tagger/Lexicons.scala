package edu.umass.cs.iesl.paperheader.tagger

//import cc.factorie.app.nlp.lexicon._
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons, TriePhraseLexicon}
import cc.factorie.app.nlp.Token
import cc.factorie.util.ModelProviderCmdOptions
import scala.collection.mutable.{HashMap, ArrayBuffer}

import scala.io.Source

/**
 * @author Kate Silverstein 
 *         created on 2/18/15
 */

class LexiconTagger(lexicon: StaticLexicons) {
  val lexiconMap = new HashMap[String, TriePhraseLexicon]()
  //authors
  lexiconMap("author1") = BibtexAuthor
  lexiconMap("author2") = BibieAuthor
  lexiconMap("person-first-highest") = lexicon.iesl.PersonFirstHighest
  lexiconMap("person-first-high") = lexicon.iesl.PersonFirstHigh
  lexiconMap("person-first-med") = lexicon.iesl.PersonFirstMedium
  lexiconMap("person-last-highest") = lexicon.iesl.PersonLastHighest
  lexiconMap("person-last-high") = lexicon.iesl.PersonLastHigh
  lexiconMap("person-last-med") = lexicon.iesl.PersonLastMedium
  lexiconMap("person-honorific") = lexicon.iesl.PersonHonorific
  //addresses
  lexiconMap("continent") = lexicon.iesl.Continents
  lexiconMap("country") = lexicon.iesl.Country
  lexiconMap("city") = lexicon.iesl.City
  lexiconMap("us-state") = lexicon.iesl.UsState
  lexiconMap("place-suffix") = lexicon.iesl.PlaceSuffix
  lexiconMap("org-suffix") = lexicon.iesl.OrgSuffix
  lexiconMap("place") = BibiePlace
  //dates
  lexiconMap("date") = BibtexDate
  lexiconMap("month") = lexicon.iesl.Month
  lexiconMap("day") = lexicon.iesl.Day
  //institution
  lexiconMap("institut") = BibieInstitution
  lexiconMap("org") = lexicon.wikipedia.Organization
  //titles
  lexiconMap("title1") = BibtexTitle
  lexiconMap("title2") = BibieTitle

  def tagTextPeople(tokens: Seq[Token], vf:  (Token => cc.factorie.variable.CategoricalVectorVar[String])): Unit = {
    val peopleLex = Set("author1", "author2", "person-first-highest", "person-first-high", "person-first-med", "person-honorific", "person-last-highest", "person-last-high", "person-last-med")
    for (k <- peopleLex) lexiconMap(k).tagText(tokens, vf, k)
  }

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

//object LexiconTagger extends ModelProviderCmdOptions {
//  val provider = new LexiconsProviderCmdOption("lexicons")
//  val sl = new StaticLexicons()(provider.value)
//
//}


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