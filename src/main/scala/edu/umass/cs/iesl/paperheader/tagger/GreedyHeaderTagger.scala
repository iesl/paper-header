package edu.umass.cs.iesl.paperheader.tagger

/**
 * Created by kate on 5/27/14.
 */

import java.io._
import java.net.URL
import cc.factorie._
import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.classify.backend.LinearMulticlassClassifier
import cc.factorie.app.nlp._
import cc.factorie.la._
import cc.factorie.optimize.Trainer
import cc.factorie.util._
import cc.factorie.variable.{CategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain}
import edu.umass.cs.iesl.paperheader.load.{LoadGrobid, PreFeatures}
import collection.mutable.ArrayBuffer
import scala.collection.mutable

class HeaderTaggerToken(val t: Token, val sentence: HeaderTaggerTokenSpan){
  lazy val string = t.string
  lazy val headerTag = t.attr[HeaderLabel]
  lazy val headerTagIntValue = if(headerTag ne null) headerTag.intValue else -1
  lazy val labeledHeaderTag = t.attr[HeaderLabel]//headerTag // todo fix this
  lazy val lemma = if(string ne null) cc.factorie.app.strings.simplifyDigits(string) else null // "form"

  //separate from lemmaLower b/c lexicons are lowercased but not simplifyDigits'ed
  lazy val lemmaNer = if (string ne null) string.toLowerCase else null

  lazy val lemmaLower = if(lemma ne null) lemma.toLowerCase else null  // "lemma"
  lazy val shape = if(lemma ne null) cc.factorie.app.strings.stringShape(lemma, 2) else ""
  //  lazy val posTagIntValue = if(posTag ne null) posTag.intValue else -1
  //  lazy val posPrunedLabels = if(lemmaLower ne null) WordData.sureAmbiguityClasses.getOrElse(lemmaLower, PosConstants.ALL_POS_LABELS) else PosConstants.ALL_POS_LABELS
//  lazy val nerPrunedLabels: Array[Int] = if (lemmaLower ne null) WordData.sureAmbiguityClasses.getOrElse(lemmaLower, HeaderTaggerConstants.ALL_HEADER_LABELS) else HeaderTaggerConstants.ALL_HEADER_LABELS
  lazy val ambiguityClass = if(lemma ne null) WordData.ambiguityClasses.getOrElse(lemmaLower, null) else null
  lazy val docFreqLemma = if(WordData.docWordCounts.contains(lemmaLower)) lemma else null
  lazy val docFreqLemma_lc = if(WordData.docWordCounts.contains(lemmaLower)) lemmaLower else null
  lazy val idxInSentence = t.positionInSentence
  lazy val idxInSection = t.positionInSection
  lazy val isPunctuation = t.isPunctuation
  lazy val grobidFeats = t.attr[PreFeatures].features
}

class HeaderTaggerTokenSpan(s: IndexedSeq[Token]){
  val length: Int = s.length
  val tokens: Array[HeaderTaggerToken] = new Array[HeaderTaggerToken](length)
  var i = 0; while(i < length) { tokens(i) = new HeaderTaggerToken(s(i), this); i += 1 }
  def apply(idx: Int) = if(idx < 0 || idx >= length) NullToken else tokens(idx)
}

object NullHeaderTag extends HeaderLabel(HeaderTaggerConstants.defaultLabel, null.asInstanceOf[Token]){
  override def categoryValue = "null"
}

object NullToken extends HeaderTaggerToken(null.asInstanceOf[Token], null.asInstanceOf[HeaderTaggerTokenSpan]){
  override lazy val string = null
//  override lazy val posTag = null
  override lazy val headerTag = NullHeaderTag
  //  override lazy val labeledPosTag = null
  override lazy val lemma = null
  override lazy val lemmaLower = null
  override lazy val shape = ""
  //  override lazy val posTagIntValue = -1
  //  override lazy val posPrunedLabels = PosConstants.ALL_POS_LABELS
  override lazy val ambiguityClass = null
  override lazy val docFreqLemma = null
  override lazy val docFreqLemma_lc = null
  override lazy val isPunctuation = true
  override lazy val grobidFeats = null
}

object HeaderTaggerConstants {
  val TOKENS_PER_DOC = 200
  final val ALL_HEADER_LABELS = (0 until HeaderLabelDomain.size).toArray
  val defaultLabel = "O"
}

object WordData {
  val ambiguityClasses: mutable.Map[String, String] = JavaHashMap[String, String]()
  val sureAmbiguityClasses: mutable.Map[String, Array[Int]] = JavaHashMap[String, Array[Int]]()
  val sureTokens: mutable.Map[String, Int] = JavaHashMap[String, Int]()
  var docWordCounts: mutable.Map[String, Int] = JavaHashMap[String, Int]()
  val observedWords: mutable.Set[String] = JavaHashSet[String]()
  val ambiguityClassThreshold = 0.4
  val toksPerDoc = 200
  var totalTokensCounted = 0
  var tokensUsingLabelPruning = 0
  var showPruning = false

  def computeWordFormsByDocFreq(tokens: Iterable[HeaderTaggerToken], cutoff: Int, toksPerDoc: Int) = {
    val docsByTokenCount = tokens.grouped(toksPerDoc)
    docsByTokenCount.foreach(doc => {
      val uniqueLemmas = doc.map(token => token.lemmaLower).toSet
      uniqueLemmas.foreach(lemma => {
        if (!docWordCounts.contains(lemma)) docWordCounts(lemma) = 1
        else docWordCounts(lemma) += 1
      })
    })
    docWordCounts = docWordCounts.filter(_._2 > cutoff)
  }

  def computeAmbiguityClasses(tokens: Iterable[HeaderTaggerToken], sureTokenThreshold: Double = 200, labelPruningThreshold: Double = 200) = {
    val tagCounts = collection.mutable.HashMap[String, Array[Int]]()
    val wordCounts = collection.mutable.HashMap[String, Double]()

    // compute word counts and per-word pos counts
    var tokenCount = 0
    tokens.foreach(t => {
      tokenCount += 1
      val lemma = t.lemmaLower
      if (!wordCounts.contains(lemma)) {
        wordCounts(lemma) = 0
        tagCounts(lemma) = Array.fill(HeaderLabelDomain.size)(0)
      }
      wordCounts(lemma) += 1
      tagCounts(lemma)(t.headerTagIntValue) += 1

      // keep track of words observed during training for computing unknown word accuracy
      // TODO should this be lemma or raw word?
      observedWords += lemma
    })

    // compute ambiguity classes from counts
    // TODO should we be doing this for only doc-frequency-filtered lemmas (current implementation) or not?
    val lemmas = docWordCounts.keySet
    lemmas.foreach(w => {
      val posFrequencies = tagCounts(w).map(_ / wordCounts(w))
      val bestPosTags = posFrequencies.zipWithIndex.filter(_._1 > ambiguityClassThreshold).unzip._2
      val ambiguityString = bestPosTags.mkString(",")
      ambiguityClasses(w) = ambiguityString
      if (wordCounts(w) >= sureTokenThreshold) {
        posFrequencies.zipWithIndex.filter(i => i._1 >= 0.9995).foreach(c => sureTokens(w) = c._2)
      }
    })
    println(s"Associated ${sureTokens.size} lemmas with a single tag:")
    sureTokens.foreach(st => {
      print(s"${st._1}/${HeaderLabelDomain.categories(st._2)} ")
    })
    println()
//    println(s"`the' counted ${wordCounts("the")} times, ${tagCounts("the")(HeaderLabelDomain.index("O"))} times as O.")

    // label pruning
    println(s"Pruning labels for lemmas seen > $labelPruningThreshold times")
    tokens.foreach(token => {
      val lemma = token.lemmaLower
      if (wordCounts(lemma) >= labelPruningThreshold && !sureTokens.contains(lemma)) {
        sureAmbiguityClasses(lemma) = tagCounts(lemma).zipWithIndex.filter(_._1 > 0).unzip._2.toArray
      }
    })
    println(s"Labels for ${sureAmbiguityClasses.size} lemmas pruned (not including sure):")
//    sureAmbiguityClasses.foreach { case (lem, classes) => print(s"($lem:${classes.map(c => HeaderLabelDomain.category(c)).mkString(",")}),") }
    println()
    println(s"Average number of labels: ${sureAmbiguityClasses.values.map(_.size).sum / sureAmbiguityClasses.size.toDouble}")
    //      println(s"${sureAmbiguityClasses.keys.mkString(" ")}")
  }
}

object HeaderFeatureDomain extends CategoricalVectorDomain[String]

class GreedyHeaderTagger extends DocumentAnnotator {

  HeaderLabelDomain += HeaderTaggerConstants.defaultLabel

  /* DocumentAnnotator methods */
  def process(document: Document): Document = {
    predict(document)
    document
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence])
  def postAttrs: Iterable[Class[_]] = List(classOf[HeaderLabel])
  def tokenAnnotationString(token: Token): String = { assert(token.nerTag != null); token.nerTag.categoryValue } // todo wrong


  class FeatureVariable(labelVariable: TokenVariable) extends BinaryFeatureVectorVariable[String] {
    def domain = HeaderFeatureDomain
    override def skipNonCategories = domain.dimensionDomain.frozen
  }
  class TokenVariable(token: HeaderTaggerToken) extends CategoricalVariable[String]{
    def domain = HeaderLabelDomain
    val target = token.headerTagIntValue
    val features = new FeatureVariable(this)
    if(HeaderFeatureDomain.dimensionDomain.frozen)
      features.set(new SparseBinaryTensor1(HeaderFeatureDomain.dimensionDomain.size))(null)
    computeFeatures(token, features)
//    println(features.activeCategories.take(10).mkString(" "))
  }

  var exampleSetsToPrediction = false
  class TokenExample(token: HeaderTaggerToken, objective: optimize.OptimizableObjectives.Multiclass) extends optimize.Example{
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
      val variable = new TokenVariable(token)
      val example = new optimize.PredictorExample(model, variable.features.value, variable.target, objective, 1.0)
      example.accumulateValueAndGradient(value, gradient)
      if (exampleSetsToPrediction) {
        token.headerTag.set(model.classification(variable.features.value).bestLabelIndex)(null)
      }
    }
  }

  class SentenceExample(sentence: HeaderTaggerTokenSpan, objective: optimize.OptimizableObjectives.Multiclass) extends optimize.Example{
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) =
      sentence.tokens.foreach(tok => {
        val tv = new TokenVariable(tok)
        new optimize.PredictorExample(model, tv.features.value, tv.target, objective, 1.0).accumulateValueAndGradient(value, gradient)
        if (exampleSetsToPrediction) {
          tok.headerTag.set(model.classification(tv.features.value).bestLabelIndex)(null)
        }
      })
  }

  lazy val model = new LinearMulticlassClassifier(HeaderLabelDomain.size, HeaderFeatureDomain.dimensionSize)

  def predict(document: Document): Unit = {
    document.sentences.foreach{sentence => {
      predict(new HeaderTaggerTokenSpan(sentence.tokens))
    }}
//    document.attr.+=(new ConllNerSpanBuffer ++= document.sections.flatMap(section => HeaderLabelDomain.spanList(section)))
  }

  def predict(sentence: HeaderTaggerTokenSpan): Unit =
    sentence.tokens.foreach{token => token.headerTag.set(model.classification(new TokenVariable(token).features.value).bestLabelIndex)(null)}


  val clusters = JavaHashMap[String, String]
  def clusterPrefix(prefixSize: Int, cluster: String): String = if (cluster.size > prefixSize) cluster.substring(0, prefixSize) else cluster

  // For words like Swedish & Swedes but not Sweden
  object Demonyms extends lexicon.PhraseLexicon("iesl/demonyms") {
    for (line <- io.Source.fromInputStream(lexicon.ClasspathResourceLexicons.getClass.getResourceAsStream("iesl/demonyms.txt")).getLines()) {
      val fields = line.trim.split(" ?\t ?") // TODO The currently checked in version has extra spaces in it; when this is fixed, use simply: ('\t')
      for (phrase <- fields.drop(1)) this += phrase
    }
  }

  /*
  RATINOV & ROTH '02
  baseline f1 testa=89.25, testb=83.65

  baseline features (sort of? based on ZJ03):
  1) previous two predictions y_i-1, y_i-2
  2) current word x_i
  3) x_i "word type" i.e. all-caps, capitalized, all-digits, alphanumeric, etc
  4) prefixes and suffixes of x_i
  5) tokens in window c = (x_i-2, x_i-1, x_i, x_i+1, x_i+2)
  6) capitalization pattern in the window c
  7) conjunction of c and y_i-1
   */

  var printFeatures = true
  var printCount = 0
  var featureConfig = ""

  // Ratinov & Roth 2009
  val contextAggMap = JavaHashMap[String,ArrayBuffer[HeaderTaggerToken]]()
  def computeFeatures(token: HeaderTaggerToken, features: FeatureVariable, useGrobidFeatures: Boolean = true) = {
    def add(feature: String): Unit = {
      features += feature
    } //featureSet += feature }

    def lemmaAtOffset(i: Int): String = {
      val idx = token.idxInSentence + i
      if (idx >= 0 || idx < token.sentence.length) cc.factorie.app.strings.simplifyDigits(token.sentence(idx).string).toLowerCase else "NAW" //FIXME this should already be a thing in Lemmas API
    }

    // word
    add("W@0=" + token.string)

    // lemma // TODO use lower case?
    add("L@0=" + token.lemma)

    // shape / punct
    add("SHAPE@0=" + cc.factorie.app.strings.stringShape(token.string, 2))
    if (token.isPunctuation) add("PUNC@0")

    // affixes
    if (token.lemma.length > 3) {
      add("PREFIX2@0=" + cc.factorie.app.strings.prefix(token.lemma, 2))
      add("SUFFIX2@0=" + cc.factorie.app.strings.suffix(token.lemma, 2))
    }
    if (token.lemma.length > 4) {
      add("PREFIX3@0=" + cc.factorie.app.strings.prefix(token.lemma, 3))
      add("SUFFIX3@0=" + cc.factorie.app.strings.suffix(token.lemma, 3))
    }
    if (token.lemma.length > 5) {
      add("PREFIX4@0=" + cc.factorie.app.strings.prefix(token.lemma, 4))
      add("SUFFIX4@0=" + cc.factorie.app.strings.suffix(token.lemma, 4))
    }

    // word, type, shape, affixes, predictions for prev, next two tokens
    val tok_2 = token.sentence(token.idxInSentence - 2)
    val tok_1 = token.sentence(token.idxInSentence - 1)
    val tok1 = token.sentence(token.idxInSentence + 1)
    val tok2 = token.sentence(token.idxInSentence + 2)
    if (tok_2.isPunctuation) add("PUNC@-2")
    if (tok_1.isPunctuation) add("PUNC@-1")
    if (tok1.isPunctuation) add("PUNC@1")
    if (tok2.isPunctuation) add("PUNC@2")

    add(s"W@-1=${tok_1.string}")
    add(s"W@-2=${tok_2.string}")
    add(s"W@1=${tok1.string}")
    add(s"W@2=${tok2.string}")
    add(s"L@-1=${tok_1.lemma}")
    add(s"L@-2=${tok_2.lemma}")
    add(s"L@1=${tok1.lemma}")
    add(s"L@2=${tok2.lemma}")
    add(s"SHAPE@-2=${tok_2.shape}")
    add(s"SHAPE@-1=${tok_1.shape}")
    add(s"SHAPE@2=${tok2.shape}")
    add(s"SHAPE@1=${tok1.shape}")
    add(s"P@-2=${tok_2.headerTag.categoryValue}")
    add(s"P@-1=${tok_1.headerTag.categoryValue}")

    // "token & previous label ngram feats"
    // I think this means some token-level ngrams combining prev label (as I have with c) and +/-2 window
    //    if (token.string.matches("[A-Za-z]+")) token.t.charNGrams(2,5).map(n => s"NGRAM@0=$n").foreach(add(_))
    add(s"W@-2=${tok_2.string}|W@-1=${tok_1.string}|W@0=${token.string}")
    add(s"P@-2=${tok_2.headerTag.categoryValue}|P@-1=${tok_1.headerTag.categoryValue}|W@0=${token.string}")
    add(s"P@-1=${tok_1.headerTag.categoryValue}|W@0=${token.string}|W@1=${tok1.string}")
    add(s"W@0=${token.string}|W@1=${tok1.string}|W@2=${tok2.string}")
    add(s"P@-1=${tok_1.headerTag.categoryValue}|W@1=${tok1.string}|W@2=${tok2.string}")

    // ambiguity classes
    add(s"A@0=${token.ambiguityClass}")
    add(s"A@1=${tok1.ambiguityClass}")
    add(s"A@2=${tok2.ambiguityClass}")
    add(s"P@-2=${tok_2.headerTag.categoryValue}|P@-1=${tok_1.headerTag.categoryValue}|A@0=${token.ambiguityClass}")
    add(s"P@-1=${tok_1.headerTag.categoryValue}|A@0=${token.ambiguityClass}|A@1=${tok1.ambiguityClass}")
    add(s"A@0=${token.ambiguityClass}|A@1=${tok1.ambiguityClass}|A@2=${tok2.ambiguityClass}")
    add(s"P@-1=${tok_1.headerTag.categoryValue}|A@1=${tok1.ambiguityClass}|A@2=${tok2.ambiguityClass}")

//     Brown clusters... size 4 looks like pos tags, so no specific pos feats
//          if (clusters.size > 0 && clusters.contains(token.string)) {
//            add("CLUS4@0=" + clusterPrefix(4, clusters(token.string)))
//            add("CLUS6@0=" + clusterPrefix(6, clusters(token.string)))
//            add("CLUS10@0=" + clusterPrefix(10, clusters(token.string)))
//            add("CLUS20@0=" + clusterPrefix(20, clusters(token.string)))
//          }
//
//          if (clusters.size > 0 && clusters.contains(tok_1.string)) {
//            add("CLUS4@-1=" + clusterPrefix(4, clusters(tok_1.string)))
//            add("CLUS6@-1=" + clusterPrefix(6, clusters(tok_1.string)))
//            add("CLUS10@-1=" + clusterPrefix(10, clusters(tok_1.string)))
//            add("CLUS20@-1=" + clusterPrefix(20, clusters(tok_1.string)))
//          }
//
//          if (clusters.size > 0 && clusters.contains(tok_2.string)) {
//            add("CLUS4@-2=" + clusterPrefix(4, clusters(tok_2.string)))
//            add("CLUS6@-2=" + clusterPrefix(6, clusters(tok_2.string)))
//            add("CLUS10@-2=" + clusterPrefix(10, clusters(tok_2.string)))
//            add("CLUS20@-2=" + clusterPrefix(20, clusters(tok_2.string)))
//          }
//
//          if (clusters.size > 0 && clusters.contains(tok1.string)) {
//            add("CLUS4@1=" + clusterPrefix(4, clusters(tok1.string)))
//            add("CLUS6@1=" + clusterPrefix(6, clusters(tok1.string)))
//            add("CLUS10@1=" + clusterPrefix(10, clusters(tok1.string)))
//            add("CLUS20@1=" + clusterPrefix(20, clusters(tok1.string)))
//          }
//
//          if (clusters.size > 0 && clusters.contains(tok2.string)) {
//            add("CLUS4@2=" + clusterPrefix(4, clusters(tok2.string)))
//            add("CLUS6@2=" + clusterPrefix(6, clusters(tok2.string)))
//            add("CLUS10@2=" + clusterPrefix(10, clusters(tok2.string)))
//            add("CLUS20@2=" + clusterPrefix(20, clusters(tok2.string)))
//          }

    // add grobid feats
    if (useGrobidFeatures) {
      // todo i think this zipWithIndex stuff is redundant now
      if (tok_1.grobidFeats != null) features ++= tok_1.grobidFeats.zipWithIndex.map { case(idx, feat) => "G" + idx + "@-1=" + feat }
      if (tok_2.grobidFeats != null) features ++= tok_2.grobidFeats.zipWithIndex.map { case(idx, feat) => "G" + idx + "@-2=" + feat }
      if (tok1.grobidFeats != null) features ++= tok1.grobidFeats.zipWithIndex.map { case(idx, feat) => "G" + idx + "@1=" + feat }
      if (tok2.grobidFeats != null) features ++= tok2.grobidFeats.zipWithIndex.map { case(idx, feat) => "G" + idx + "@2=" + feat }
    }

    // add old HeaderTagger feats
    // todo extract feats from TokenFeatures that are not already here
    features ++= TokenFeatures(token.t)

    // NOT INCLUDED since they require first tagging the corpus with a baseline model!
    // 1. token-majority relative frequency
    // majority label assigned to this token in 1k token window (+/- 500)
    // 2. entity-majority relative frequency
    // majority label assigned to this entity (span) in 1k token window (+/- 500)
    def addLexiconFeats(tok: HeaderTaggerToken, text: String) = {
      if (lexicon.iesl.Month.containsLemmatizedWord(tok.lemmaNer)) add("MONTH@" + text)
      if (lexicon.iesl.Day.containsLemmatizedWord(tok.lemmaNer)) add("DAY@" + text)

      if (lexicon.iesl.PersonFirst.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-FIRST@" + text)
      if (lexicon.iesl.PersonFirstHigh.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-FIRST-HIGH@" + text)
      if (lexicon.iesl.PersonFirstHighest.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-FIRST-HIGHEST@" + text)
      if (lexicon.iesl.PersonFirstMedium.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-FIRST-MEDIUM@" + text)

      if (lexicon.iesl.PersonLast.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-LAST@" + text)
      if (lexicon.iesl.PersonLastHigh.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-LAST-HIGH@" + text)
      if (lexicon.iesl.PersonLastHighest.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-LAST-HIGHEST@" + text)
      if (lexicon.iesl.PersonLastMedium.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-LAST-MEDIUM@" + text)

      if (lexicon.iesl.PersonHonorific.containsLemmatizedWord(tok.lemmaNer)) add("PERSON-HONORIFIC@" + text)

      if (lexicon.iesl.Company.containsLemmatizedWord(tok.lemmaNer)) add("COMPANY@" + text)
      if (lexicon.iesl.JobTitle.containsLemmatizedWord(tok.lemmaNer)) add("JOB-TITLE@" + text)
      if (lexicon.iesl.OrgSuffix.containsLemmatizedWord(tok.lemmaNer)) add("ORG-SUFFIX@" + text)

      if (lexicon.iesl.Country.containsLemmatizedWord(tok.lemmaNer)) add("COUNTRY@" + text)
      if (lexicon.iesl.City.containsLemmatizedWord(tok.lemmaNer)) add("CITY@" + text)
      if (lexicon.iesl.PlaceSuffix.containsLemmatizedWord(tok.lemmaNer)) add("PLACE-SUFFIX@" + text)
      if (lexicon.iesl.USState.containsLemmatizedWord(tok.lemmaNer)) add("USSTATE@" + text)
      if (lexicon.iesl.Continents.containsLemmatizedWord(tok.lemmaNer)) add("CONTINENT@" + text)

      if (lexicon.wikipedia.Person.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-PERSON@" + text)
      if (lexicon.wikipedia.Event.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-EVENT@" + text)
      if (lexicon.wikipedia.Location.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-LOCATION@" + text)
      if (lexicon.wikipedia.Organization.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-ORG@" + text)
      if (lexicon.wikipedia.ManMadeThing.containsLemmatizedWord(tok.lemmaNer)) add("MANMADE@" + text)
      if (Demonyms.containsLemmatizedWord(tok.lemmaNer)) add("DEMONYM@" + text)

      if (lexicon.wikipedia.Book.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-BOOK@" + text)
      if (lexicon.wikipedia.Business.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-BUSINESS@" + text)
      if (lexicon.wikipedia.Film.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-FILM@" + text)

      if (lexicon.wikipedia.LocationAndRedirect.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-LOCATION-REDIRECT@" + text)
      if (lexicon.wikipedia.PersonAndRedirect.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-PERSON-REDIRECT@" + text)
      if (lexicon.wikipedia.OrganizationAndRedirect.containsLemmatizedWord(tok.lemmaNer)) add("WIKI-ORG-REDIRECT@" + text)
    }

    def addLexiconBigramFeats(tok1: HeaderTaggerToken, tok2: HeaderTaggerToken, text: String) = {
      if (lexicon.iesl.Company.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("COMPANY@" + text)
      if (lexicon.iesl.JobTitle.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("JOB-TITLE@" + text)

      if (lexicon.iesl.Country.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("COUNTRY@" + text)
      if (lexicon.iesl.City.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("CITY@" + text)
      if (lexicon.iesl.PlaceSuffix.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("PLACE-SUFFIX@" + text)
      if (lexicon.iesl.USState.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("USSTATE@" + text)
      if (lexicon.iesl.Continents.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("CONTINENT@" + text)

      if (lexicon.wikipedia.Person.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-PERSON@" + text)
      if (lexicon.wikipedia.Event.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-EVENT@" + text)
      if (lexicon.wikipedia.Location.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-LOCATION@" + text)
      if (lexicon.wikipedia.Organization.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-ORG@" + text)
      if (lexicon.wikipedia.ManMadeThing.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("MANMADE@" + text)
      if (Demonyms.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("DEMONYM@" + text)

      if (lexicon.wikipedia.Book.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-BOOK@" + text)
      if (lexicon.wikipedia.Business.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-BUSINESS@" + text)
      if (lexicon.wikipedia.Film.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-FILM@" + text)

      if (lexicon.wikipedia.LocationAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-LOCATION-REDIRECT@" + text)
      if (lexicon.wikipedia.PersonAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-PERSON-REDIRECT@" + text)
      if (lexicon.wikipedia.OrganizationAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer))) add("WIKI-ORG-REDIRECT@" + text)
    }

    def addLexiconTrigramFeats(tok1: HeaderTaggerToken, tok2: HeaderTaggerToken, tok3: HeaderTaggerToken, text: String) = {
      if (lexicon.iesl.Company.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("COMPANY@" + text)
      if (lexicon.iesl.JobTitle.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("JOB-TITLE@" + text)

      if (lexicon.iesl.Country.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("COUNTRY@" + text)
      if (lexicon.iesl.City.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("CITY@" + text)
      if (lexicon.iesl.PlaceSuffix.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("PLACE-SUFFIX@" + text)
      if (lexicon.iesl.USState.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("USSTATE@" + text)
      if (lexicon.iesl.Continents.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("CONTINENT@" + text)

      if (lexicon.wikipedia.Person.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-PERSON@" + text)
      if (lexicon.wikipedia.Event.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-EVENT@" + text)
      if (lexicon.wikipedia.Location.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-LOCATION@" + text)
      if (lexicon.wikipedia.Organization.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-ORG@" + text)
      if (lexicon.wikipedia.ManMadeThing.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("MANMADE@" + text)
      if (Demonyms.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("DEMONYM@" + text)

      if (lexicon.wikipedia.Book.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-BOOK@" + text)
      if (lexicon.wikipedia.Business.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-BUSINESS@" + text)
      if (lexicon.wikipedia.Film.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-FILM@" + text)

      if (lexicon.wikipedia.LocationAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-LOCATION-REDIRECT@" + text)
      if (lexicon.wikipedia.PersonAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-PERSON-REDIRECT@" + text)
      if (lexicon.wikipedia.OrganizationAndRedirect.containsLemmatizedWords(Seq(tok1.lemmaNer, tok2.lemmaNer, tok3.lemmaNer))) add("WIKI-ORG-REDIRECT@" + text)
    }

    // lexicons
//    addLexiconFeats(token, "0")
//    addLexiconBigramFeats(tok_1, token, "-10")
//    addLexiconBigramFeats(token, tok1, "01")
//    addLexiconTrigramFeats(tok_2, tok_1, token, "-2-10")
//    addLexiconTrigramFeats(tok_1, token, tok1, "-101")
//    addLexiconTrigramFeats(token, tok1, tok2, "012")
  }

  def tokenFeaturesString(tokens:Iterable[Token]): String = tokens.map(token => "%-20s  %s".format(token.string, token.attr[FeatureVariable])).mkString("\n")

  def sampleOutputString(tokens:Iterable[Token]): String = {
    val sb = new StringBuffer
    for (token <- tokens)
      sb.append("%s %20s %10s %10s  %s\n".format(if (token.attr[HeaderLabel].valueIsTarget) " " else "*", token.string, token.attr[HeaderLabel].target.categoryValue, token.attr[HeaderLabel].categoryValue, token.attr[FeatureVariable]))
    sb.toString
  }

  def accuracy(docs: Iterable[Document]): (Double, Double, Double, Double) = {
    var tokenTotal, tokenCorrect, totalTime, docTotal, docCorrect = 0.0
    require(docs.toSeq.length > 0, { println("accuracy: docs.length < 0")})
    docs.foreach(doc => {
      var thisDocCorrect = 1.0
      val t0 = System.currentTimeMillis()
      process(doc)
      totalTime += System.currentTimeMillis() - t0
      val tokens = doc.sections.flatMap(_.tokens)
      for (token <- tokens) {
        tokenTotal += 1
        if (token.attr[HeaderLabel].valueIsTarget) tokenCorrect += 1.0
        else thisDocCorrect = 0.0
      }
      docCorrect += thisDocCorrect
      docTotal += 1.0
    })
    val tokensPerSecond = (tokenTotal / totalTime) * 1000.0
    val tokenAcc = tokenCorrect / tokenTotal
    val docAcc = docCorrect / docTotal
    (tokenAcc, docAcc, tokensPerSecond, tokenTotal)
  }

  def printAccuracy(docs: Iterable[Document], extraText: String) = {
    val (tokAcc, docAcc, speed, _) = accuracy(docs)
    println(s"$extraText $tokAcc token accuracy, $docAcc document accuracy, $speed tokens/sec")
  }

  def train(trainDocs: Iterable[Document], testDocs: Iterable[Document], opts: HeaderTaggerOpts)(implicit random: scala.util.Random): Double = {

    val trainSentences = trainDocs.flatMap(_.sentences.map(sent => new HeaderTaggerTokenSpan(sent.tokens)))//.shuffle
    val trainTokens = trainSentences.flatMap(_.tokens)
    val testLabels = testDocs.flatMap(doc => doc.tokens.map(_.attr[HeaderLabel]))

    println(s"HeaderLabel domain size: ${HeaderLabelDomain.size}")

    println("Computing WordData...")
    //    WordData.computeWordPOSDistr(tokens)
    //    WordData.computeWordFormsByDocFreq(tokens, 1, toksPerDoc)
    //    WordData.computeAmbiguityClasses(tokens)
    //    WordData.computeLexClasses(tokens)
    WordData.computeWordFormsByDocFreq(trainTokens, 1, HeaderTaggerConstants.TOKENS_PER_DOC)
    WordData.computeAmbiguityClasses(trainTokens)

//    val objective = if(useHingeLoss) optimize.OptimizableObjectives.hingeMulticlass else  optimize.OptimizableObjectives.sparseLogMulticlass

    // todo make these command line opts
    val objective = optimize.OptimizableObjectives.hingeMulticlass
    val cutoff = 2

    println("Generating feature domain")
    HeaderFeatureDomain.dimensionDomain.gatherCounts = true
    trainSentences.map(_.tokens.map(tok => new TokenVariable(tok)))
    println(s"Feature count before cutoff: ${HeaderFeatureDomain.dimensionDomain.size}")
    HeaderFeatureDomain.dimensionDomain.trimBelowCount(cutoff)
    HeaderFeatureDomain.freeze()
    println(s"After pruning (cutoff=$cutoff) using ${HeaderFeatureDomain.dimensionDomain.size} features.")

    def evaluate() = {
      //      exampleSetsToPrediction = doBootstrap
//      printAccuracy(trainDocs, "Training: ")
      if(testDocs.size > 0) {
        printAccuracy(testDocs, "Testing: ")
        println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, testLabels.toIndexedSeq))
      }
      println(s"Sparsity: ${model.weights.value.toSeq.count(_ == 0).toFloat/model.weights.value.length}")
    }

    /* Each token is an example */
//    val examples = trainSentences.flatMap(sentence => sentence.tokens.map(tok => new TokenExample(tok, objective))).toIndexedSeq
    /* Each sentence is an example */
    val examples = trainSentences.map{sentence => new SentenceExample(sentence, objective)}.toIndexedSeq

    /* Each sentence is an example */
    //    val examples = trainSentences.map(sentence => new SentenceExample(sentence, objective)).toIndexedSeq

    val optimizer = new optimize.AdaGradRDA(delta=opts.delta.value, rate=opts.learningRate.value, l1=opts.l1.value, l2=opts.l2.value, numExamples=examples.length)

    println("Training...")
    Trainer.onlineTrain(model.parameters, examples, maxIterations=opts.numIterations.value, optimizer=optimizer, evaluate=evaluate, useParallelTrainer=if (opts.nThreads.value > 1) true else false, nThreads=opts.nThreads.value)

    // return f1
//    println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, testLabels.toIndexedSeq))
    val trainLabels = trainDocs.flatMap(doc => doc.tokens.map(_.attr[HeaderLabel]))
    printAccuracy(trainDocs, "Train:")
    val trainEval =  new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, trainLabels.toIndexedSeq)
    println(trainEval)
    trainEval.f1
  }

  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = this(url.openConnection.getInputStream)

  // Serialization
  def serialize(filename: String): Unit = {
    val file = new File(filename); if (file.getParentFile ne null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import CubbieConversions._
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(model.weights.value.dim1, model.weights.value.dim2, new la.SparseIndexedTensor1(_))
    model.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderFeatureDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    //    BinarySerializer.serialize(WordData.wordPosMax, dstream)
    //        BinarySerializer.serialize(WordData.ambiguityClasses, dstream)
    //    BinarySerializer.serialize(WordData.sureTokens, dstream)
    //    BinarySerializer.serialize(WordData.docWordCounts, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderFeatureDomain.dimensionDomain, dstream)
    model.weights.set(new la.DenseLayeredTensor2(HeaderFeatureDomain.dimensionDomain.size, HeaderLabelDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    //    BinarySerializer.deserialize(WordData.wordPosMax, dstream)
    //    BinarySerializer.deserialize(WordData.ambiguityClasses, dstream)
    //    BinarySerializer.deserialize(WordData.sureTokens, dstream)
    //    BinarySerializer.deserialize(WordData.docWordCounts, dstream)
    HeaderFeatureDomain.freeze()
    HeaderLabelDomain.freeze()
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }

}

object TrainGreedyHeaderTagger extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => trainGrobid(opts)
      case _ => trainDefault(opts)
    }
  }

  def trainDefault(opts: HeaderTaggerOpts): Double = throw new Exception("not yet implemented.")

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    import edu.umass.cs.iesl.paperheader.load.LoadGrobid
    def initGrobidFeatures(docs: Seq[Document]): Unit = {
      docs.flatMap(_.tokens).foreach { token =>
        token.attr += new HeaderFeatures(token)
        token.attr[HeaderFeatures] ++= token.attr[PreFeatures].features
      }
    }
    implicit val random = new scala.util.Random
//    val params = new HyperParams(opts)
//    println(params)
    val tagger = new GreedyHeaderTagger
    val allData = LoadGrobid.fromFilename(opts.trainFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)
    HeaderLabelDomain.freeze()
    println("using labels: " + HeaderLabelDomain.categories.mkString(", "))
    val trainPortion = (allData.length.toDouble * opts.trainPortion.value).floor.toInt
    val devPortion = (allData.length.toDouble * (if(opts.testPortion.wasInvoked) opts.testPortion.value else 1.0-opts.trainPortion.value)).floor.toInt
    val trainingData = allData.take(trainPortion)
    val devData = allData.drop(trainPortion).take(devPortion)

    val testData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)

    println(s"training data: ${trainingData.length} docs, ${trainingData.flatMap(_.tokens).length} tokens")
    println(s"dev data: ${devData.length} docs, ${devData.flatMap(_.tokens).length} tokens")

    trainingData.head.tokens.take(5).foreach { t => println(s"${t.string} ${t.attr[HeaderLabel].categoryValue}")}

    // initialize features
//    trainingData.foreach(doc => tagger.addFeatures(doc, opts.useGrobidFeatures.value))
//    HeaderFeatureDomain.freeze()
//    testingData.foreach(doc => tagger.addFeatures(doc, opts.useGrobidFeatures.value))

//    println(s"feature domain size: ${HeaderFeatureDomain.dimensionDomain.size}")
//    trainingData.head.tokens.take(5).foreach { t => println(s"${t.attr[HeaderFeatures]}")}
    val f1 = tagger.train(trainingData, devData, opts)
    if (opts.saveModel.value) {
      println(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }

    tagger.printAccuracy(testData, "Test: ")
    println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, testData.flatMap{doc => doc.tokens.map{_.attr[HeaderLabel]}}.toIndexedSeq))

    val outputFname = "test-output"
    println(s"writing tagged output to $outputFname")
    val writer = new PrintWriter(outputFname)
    testData.foreach{doc => {
      doc.tokens.foreach{token =>{
        val label = token.attr[HeaderLabel]
        writer.println(s"${token.string}\t${label.target.categoryValue}\t${label.categoryValue}")
      }}
      writer.println()
    }}
    writer.close()

    //    val evaluator = new ExactlyLikeGrobidEvaluator
    //    val (f0, eval) = evaluator.evaluate(testingData, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    //    println(eval)
    //    f0
    f1
  }
}

object OptimizeGreedyCitationModel {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.saveModel.setValue(false)
    opts.writeEvals.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 10))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 10))
    val rate = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-4, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 1))

    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.paperheader.tagger.TrainGreedyHeaderTagger")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.saveModel.setValue(true)
    opts.writeEvals.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}

object TestGreedyHeaderTagger {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => processGrobid(opts)
      case _ => processDefault(opts)
    }
  }
  def processDefault(opts: HeaderTaggerOpts): Unit = throw new Exception("not yet implemented")
  def processGrobid(opts: HeaderTaggerOpts): Unit = {
    val trainer = new GreedyHeaderTagger
    println(s"loading file: ${opts.testFile.value} with features? ${opts.useGrobidFeatures.value}")
    val testingData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value, bilou=opts.bilou.value)
//    testingData.foreach(doc => trainer.addFeatures(doc, useGrobidFeatures = opts.useGrobidFeatures.value))
    trainer.deserialize(new URL(opts.modelFile.value).openStream())
    println(s"label domain size: ${HeaderLabelDomain.dimensionDomain.size}")
    println(s"feature domain size: ${HeaderFeatureDomain.dimensionDomain.size}")
    val tot = trainer.model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = trainer.model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    println(s"model sparsity: $sparsity")
    val labels = testingData.flatMap(_.tokens).map(_.attr[HeaderLabel])
    trainer.printAccuracy(testingData, "Testing: ")
//    testingData.foreach(trainer.process)
    val segEval = new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, labels.toIndexedSeq)
    println(segEval)
    val evaluator = new ExactlyLikeGrobidEvaluator
    val (_, eval) = evaluator.evaluate(testingData, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    println(eval)
  }
}
