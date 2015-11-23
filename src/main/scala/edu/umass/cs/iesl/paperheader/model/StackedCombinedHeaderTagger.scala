package edu.umass.cs.iesl.paperheader.model

import java.util.logging.Logger

import cc.factorie.app.nlp.lemma.{LowercaseTokenLemma, LowercaseLemmatizer}
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.embeddings.SkipGramEmbedding
import cc.factorie.app.nlp.lexicon.StaticLexicons
import cc.factorie.util.JavaHashMap
import cc.factorie.variable.CategoricalVectorVar

import scala.math._

/**
 * Created by kate on 11/18/15.
 */
class StackedCombinedHeaderTagger(lexicon: StaticLexicons, rlog: Option[Logger], params: Hyperparams, embeddingMap: SkipGramEmbedding = null)
  extends StackedChainHeaderTagger(rlog, params, embeddingMap) {

  private val log = Logger.getLogger(getClass.getName)

  lexicon.synchronized {
    lexicon.iesl.Month.toString()
    lexicon.iesl.Day.toString()
    lexicon.iesl.PersonFirst.toString()
    lexicon.iesl.PersonFirstHigh.toString()
    lexicon.iesl.PersonFirstHighest.toString()
    lexicon.iesl.PersonFirstMedium.toString()
    lexicon.iesl.PersonLast.toString()
    lexicon.iesl.PersonLastHigh.toString()
    lexicon.iesl.PersonLastHighest.toString()
    lexicon.iesl.PersonLastMedium.toString()
    lexicon.iesl.PersonHonorific.toString()
    lexicon.iesl.Company.toString()
    lexicon.iesl.JobTitle.toString()
    lexicon.iesl.OrgSuffix.toString()
    lexicon.iesl.Country.toString()
    lexicon.iesl.City.toString()
    lexicon.iesl.PlaceSuffix.toString()
    lexicon.iesl.UsState.toString()
    lexicon.iesl.Continents.toString()
    lexicon.wikipedia.Person.toString()
    lexicon.wikipedia.Event.toString()
    lexicon.wikipedia.Location.toString()
    lexicon.wikipedia.Organization.toString()
    lexicon.wikipedia.ManMadeThing.toString()
    lexicon.iesl.Demonym.toString()
    lexicon.wikipedia.Book.toString()
    lexicon.wikipedia.Business.toString()
    lexicon.wikipedia.Film.toString()
    lexicon.wikipedia.LocationAndRedirect.toString()
    lexicon.wikipedia.PersonAndRedirect.toString()
    lexicon.wikipedia.OrganizationAndRedirect.toString()
    log.info("loaded lexicons")
  }

  val FEATURE_PREFIX_REGEX = "^[^@]*$".r

  def addFeatures1(doc: Document, vf: Token => CategoricalVectorVar[String]): Unit = {
    LowercaseLemmatizer.process(doc)
    val lemmaFxn = (t: Token) => t.attr[LowercaseTokenLemma].value
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach { token =>
      val fv = new FeatureVar(token)
      val grobidFeatures = token.attr[GrobidFeatures].features
      grobidFeatures.zipWithIndex.foreach { case (fval, idx) => fv += s"G_$idx=$fval" }
      fv ++= FeatureExtractor.firstOrderFeatures(token)
      token.attr += fv
    }
    val vf = (t: Token) => t.attr[FeatureVar]
    lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH", lemmaFxn)
    lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY", lemmaFxn)
    lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST", lemmaFxn)
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH", lemmaFxn)
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST", lemmaFxn)
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM", lemmaFxn)
    lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST", lemmaFxn)
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH", lemmaFxn)
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST", lemmaFxn)
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM", lemmaFxn)
    lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC", lemmaFxn)
    lexicon.iesl.Company.tagText(tokenSequence,vf, "COMPANY", lemmaFxn)
    lexicon.iesl.JobTitle.tagText(tokenSequence,vf, "JOB-TITLE", lemmaFxn)
    lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf, "ORG-SUFFIX", lemmaFxn)
    lexicon.iesl.Country.tagText(tokenSequence,vf, "COUNTRY", lemmaFxn)
    lexicon.iesl.City.tagText(tokenSequence,vf, "CITY", lemmaFxn)
    lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf, "PLACE-SUFFIX", lemmaFxn)
    lexicon.iesl.UsState.tagText(tokenSequence,vf, "USSTATE", lemmaFxn)
    lexicon.iesl.Continents.tagText(tokenSequence,vf, "CONTINENT", lemmaFxn)
    lexicon.wikipedia.Person.tagText(tokenSequence,vf, "WIKI-PERSON", lemmaFxn)
    lexicon.wikipedia.Event.tagText(tokenSequence,vf, "WIKI-EVENT", lemmaFxn)
    lexicon.wikipedia.Location.tagText(tokenSequence,vf, "WIKI-LOCATION", lemmaFxn)
    lexicon.wikipedia.Organization.tagText(tokenSequence,vf, "WIKI-ORG", lemmaFxn)
    lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf, "MANMADE", lemmaFxn)
    lexicon.iesl.Demonym.tagText(tokenSequence,vf, "DEMONYM", lemmaFxn)
    lexicon.wikipedia.Book.tagText(tokenSequence,vf, "WIKI-BOOK", lemmaFxn)
    lexicon.wikipedia.Business.tagText(tokenSequence,vf, "WIKI-BUSINESS", lemmaFxn)
    lexicon.wikipedia.Film.tagText(tokenSequence,vf, "WIKI-FILM", lemmaFxn)
    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf, "WIKI-LOCATION-REDIRECT", lemmaFxn)
    lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf, "WIKI-PERSON-REDIRECT", lemmaFxn)
    lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf, "WIKI-ORG-REDIRECT", lemmaFxn)
    cc.factorie.app.chain.Observations.addNeighboringFeatures(tokenSequence.toIndexedSeq, vf, FEATURE_PREFIX_REGEX, -2, 2)
  }

  class TokenSequence(token: Token) extends collection.mutable.ArrayBuffer[Token] {
    this.prepend(token)
    val label : String = token.attr[HeaderTag].categoryValue.split("-")(1)
    def key = this.mkString("-")
  }

  def getSequences(document : Document) : List[TokenSequence] = {
    var sequences = List[TokenSequence]()
    var seq : TokenSequence = null
    for (token <- document.tokens) {
      val categoryVal = token.attr[HeaderTag].categoryValue
      if (categoryVal.length() > 0) {
        categoryVal.substring(0,1) match {
          case "B" => seq = new TokenSequence(token)
          case "I" => if (seq != null) seq.append(token) else seq = new TokenSequence(token)
          case "U" => seq = new TokenSequence(token)
          case "L" => if (seq != null) seq.append(token) else seq = new TokenSequence(token)
          case _ => null
        }
        if (categoryVal.matches("(L|U)-\\D+")) sequences = seq :: sequences
      }
    }
    sequences
  }

  def allSubstrings(seq: TokenSequence, length : Int) : List[String] = {
    if(length == 0) return List[String]()
    var list = List[String]()
    for(i <- 0 to seq.length-length) {
      var sub = ""
      for(k <- i until i+length) {
        sub += " " + seq(k).string
      }
      list = sub :: list
    }
    allSubstrings(seq, length-1) ::: list
  }

  def mode(list : List[String]) : String = {
    val domainCount = new collection.mutable.HashMap[String, Int]
    for(item <- list) {
      if(domainCount.contains(item)) domainCount(item) = domainCount(item) + 1
      else domainCount(item) = 1
    }
    var maxDomain = ""
    var maxCount = 0
    for(domain <- domainCount.keys) {
      if(domainCount(domain) > maxCount) {
        maxCount = domainCount(domain)
        maxDomain = domain
      }
    }
    maxDomain
  }

  def history(list : List[String], category : String) : String = {
    (round( 10.0 * ((list.count(_ == category).toDouble / list.length.toDouble)/3)) / 10.0).toString
  }

  def history(count : Int, total : Int) : String = {
    (round( 10.0 * ((count.toDouble / total)/3.0)) / 10.0).toString
  }

  def addFeatures2(doc: Document): Unit = {
    val tokenSeq = doc.tokens.toIndexedSeq
    val n = tokenSeq.length
    var i = 0
    while (i < n) {
      val t = tokenSeq(i)
      i += 1
      val tokenPrevWindow = t.prevWindow(2)
      t.attr[FeatureVar2] ++= tokenPrevWindow.zipWithIndex.map(t2 => "PREVLABEL" + t2._2 + "="+t2._1.attr[HeaderTag].categoryValue)
      if (t.hasPrev) {
        t.attr[FeatureVar2] += "PREVLABELCON=" + t.prev.attr[HeaderTag].categoryValue + "&" + t.string
      }
      if (t.sentenceHasPrev) {
        t.attr[FeatureVar2] ++= tokenPrevWindow.map(t2 => "PREVLABELLCON=" + t.sentencePrev.attr[HeaderTag].categoryValue + "&" + t2.string)
        t.attr[FeatureVar2] ++= t.nextWindow(2).map(t2 => "PREVLABELLCON=" + t.sentencePrev.attr[HeaderTag].categoryValue + "&" + t2.string)
      }
    }

    val sequences = getSequences(doc)
    val tokenToLabelMap = JavaHashMap[String,List[String]]()
    val sequenceToLabelMap = JavaHashMap[String,List[String]]()
    val subsequencesToLabelMap = JavaHashMap[String,List[String]]()
    i = 0
    while (i < n) {
      val token = tokenSeq(i)
      i += 1
      if (tokenToLabelMap.contains(token.string))
        tokenToLabelMap(token.string) = tokenToLabelMap(token.string) ++ List(token.attr[HeaderTag].categoryValue)
      else
        tokenToLabelMap(token.string) = List(token.attr[HeaderTag].categoryValue)
    }
    for (seq <- sequences) {
      if(sequenceToLabelMap.contains(seq.key))
        sequenceToLabelMap(seq.key) = sequenceToLabelMap(seq.key) ++ List(seq.label)
      else
        sequenceToLabelMap(seq.key) = List(seq.label)
    }
    for (seq <- sequences) {
      for(subseq <- allSubstrings(seq, seq.length)) {
        if(subsequencesToLabelMap.contains(subseq))
          subsequencesToLabelMap(subseq) = subsequencesToLabelMap(subseq) ++ List(seq.label)
        else
          subsequencesToLabelMap(seq.key) = List(seq.label)
      }
    }
    for (token <- tokenSeq) {
      val tokenVote = tokenToLabelMap(token.string)
      token.attr[FeatureVar2] += "CLASSIFIERLABEL="+mode(tokenVote)
    }
    for(seq <- sequences) {
      val seqVote = sequenceToLabelMap(seq.key)
      val seqLabelMode = mode(seqVote)
      val subSeqVote = subsequencesToLabelMap(seq.key)
      val subSeqLabelMode = mode(subSeqVote)
      for(token <- seq) {
        token.attr[FeatureVar2] += "SEQUENCELABEL="+seqLabelMode
        token.attr[FeatureVar2] += "SUBSEQUENCELABEL="+subSeqLabelMode
      }
    }

    val extendedPrediction = JavaHashMap[String, collection.mutable.Map[String,Int]]()
    val surfaceFormCount = JavaHashMap[String,Int]()
    for(token <- tokenSeq) {
      val tokenStr = token.string
      if(extendedPrediction.contains(tokenStr)) {
        HeaderDomain.categories.foreach { str =>
          token.attr[FeatureVar2] += str + "=" +
            history(extendedPrediction(token.string).getOrElse(str,0), surfaceFormCount.getOrElse(tokenStr,0))
        }
        val map = extendedPrediction(tokenStr)
        val count = map.getOrElse(token.attr[HeaderTag].categoryValue,0) + 1
        map.put(token.attr[HeaderTag].categoryValue,count)
        surfaceFormCount.put(tokenStr,surfaceFormCount.getOrElse(tokenStr,0) + 1)
      } else {
        val map = JavaHashMap[String,Int]()
        map.put(token.attr[HeaderTag].categoryValue,1)
        extendedPrediction.put(tokenStr,map)
        surfaceFormCount.put(tokenStr,1)
      }
    }

    val clusters = FeatureExtractor.clusters
    if (clusters.nonEmpty) {
      for(token <- tokenSeq) {
        val rawWord = token.string
        if(token.hasPrev) {
          if(clusters.contains(rawWord))
            token.attr[FeatureVar2] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[HeaderTag].categoryValue + "&" + FeatureExtractor.prefix(_,clusters(rawWord)))
          if(token.hasNext) {
            var nextRawWord = token.next.string
            if(clusters.contains(nextRawWord))
              token.attr[FeatureVar2] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[HeaderTag].categoryValue + "&" + FeatureExtractor.prefix(_,clusters(nextRawWord)))
            if(token.next.hasNext && clusters.contains(token.next.next.string)) {
              nextRawWord = token.next.next.string
              token.attr[FeatureVar2] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[HeaderTag].categoryValue + "&" + FeatureExtractor.prefix(_,clusters(nextRawWord)))
            }
          }
          var prevRawWord = token.prev.string
          if(clusters.contains(prevRawWord))
            token.attr[FeatureVar2] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[HeaderTag].categoryValue + "&" + FeatureExtractor.prefix(_,clusters(prevRawWord)))
          if(token.prev.hasPrev && clusters.contains(token.prev.prev.string)) {
            prevRawWord = token.prev.prev.string
            token.attr[FeatureVar2] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[HeaderTag].categoryValue + "&" + FeatureExtractor.prefix(_,clusters(prevRawWord)))
          }
        }
      }
    }

  }

}
