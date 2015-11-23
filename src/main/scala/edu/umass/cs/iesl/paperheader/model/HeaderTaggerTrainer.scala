package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.logging.{FileHandler, Logger}

import cc.factorie._
import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.util.HyperparameterMain
import edu.umass.cs.iesl.paperheader._
import edu.umass.cs.iesl.paperheader.load.{LoadGrobid, LoadIESL}
import edu.umass.cs.iesl.paperheader.util.Util

import scala.io.Source

/**
 * Created by kate on 11/14/15.
 */
object HeaderTaggerTrainer extends HyperparameterMain {

  private val log = Logger.getLogger(getClass.getName)

  def evaluateParameters(args: Array[String]): Double = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    log.info(s"${opts.unParse.mkString(",")}")
    opts.taggerType.value match {
      case TAGGER_TYPE_DEFAULT => trainDefault(opts)
      case TAGGER_TYPE_GROBID => trainGrobid(opts)
      case TAGGER_TYPE_COMBINED => trainCombined(opts)
      case _ => throw new Exception(s"bad tagger-type option: ${opts.taggerType.value}")
    }
  }

  def trainDefault(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val resultsLog = Util.getLog("DefaultHeaderTaggerResults")
    resultsLog.info(s"${opts.unParse.mkString(",")}")
    val trainDocs = LoadIESL.fromFilename(opts.trainFile.value).shuffle
    val devDocs = LoadIESL.fromFilename(opts.devFile.value).shuffle
    initDomain()
    HeaderDomain.freeze()
    val headerDomainInfo = domainInfo()
    log.info(headerDomainInfo)
    resultsLog.info(headerDomainInfo)
    val params = Hyperparams(opts)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new DefaultHeaderTagger(Some(resultsLog), lexicon)
    val result = tagger.train(trainDocs, devDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    /* TODO somethings wrong with deserialization */
    val testDocs = LoadIESL.fromFilename(opts.testFile.value)
    val labels = testDocs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    testDocs.foreach(tagger.process)
    resultsLog.info("TEST:")
    resultsLog.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels).toString())
    result
  }

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val resultsLog = Util.getLog("GrobidHeaderTaggerResults")
    resultsLog.info(s"${opts.unParse.mkString(",")}")
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value).shuffle
    initDomain()
    HeaderDomain.freeze()
    val headerDomainInfo = domainInfo()
    log.info(headerDomainInfo)
    resultsLog.info(headerDomainInfo)
    val params = Hyperparams(opts)
    val tagger = new GrobidHeaderTagger(Some(resultsLog))
    val result = tagger.train(trainDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    /* TODO somethings wrong with deserialization */
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    val labels = testDocs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    testDocs.foreach(tagger.process)
    resultsLog.info("TEST:")
    resultsLog.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels).toString())
    result
  }

  def trainCombined(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val resultsLog = Util.getLog("CombinedHeaderTaggerResults")
    resultsLog.info(s"${opts.unParse.mkString(",")}")

    if (opts.brownClusters.wasInvoked) {
      val bcFile = opts.brownClusters.value
      log.info(s"loading brown clusters from $bcFile")
      val lines = Source.fromFile(bcFile).getLines()
      while (lines.hasNext) {
        val line = lines.next()
        val splitLine = line.split("\t")
        FeatureExtractor.clusters(splitLine(1)) = splitLine(0)
      }
    }

    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value).shuffle
    initDomain()
    HeaderDomain.freeze()
    val headerDomainInfo = domainInfo()
    log.info(headerDomainInfo)
    resultsLog.info(headerDomainInfo)
    val params = Hyperparams(opts)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new CombinedHeaderTagger(Some(resultsLog), lexicon)
    val result = tagger.train(trainDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    /* TODO somethings wrong with deserialization */
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    val labels = testDocs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    testDocs.foreach(tagger.process)
    resultsLog.info("TEST:")
    resultsLog.info(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels).toString())
    result
  }

  def initDomain(): Unit = {
    val baseCats = HeaderDomain.categories.map { c => c.split("-").last }.toSet
    for (c <- baseCats) {
      for (prefix <- List("B", "I", "L", "U")) {
        val bilou = s"$prefix-$c"
        if (!HeaderDomain.contains(bilou)) HeaderDomain += bilou
      }
    }
    if (!HeaderDomain.contains("O")) HeaderDomain += "O"
  }

  def domainInfo(): String = {
    val sb = new StringBuilder
    sb.append(s"HeaderDomain: size=${HeaderDomain.size}\n")
    val cats = HeaderDomain.categories.sortBy { c => c.split("-").last }.mkString(",")
    sb.append(s"HeaderDomain: categories=$cats")
    sb.toString()
  }

  def check(docs: Seq[Document]): Unit = {
    docs.take(3).foreach { doc =>
      doc.tokens.foreach { token =>
        println(s"${token.string}\t${token.attr[GoldHeaderTag].target.categoryValue}")
      }
      println("")
    }
  }

}
