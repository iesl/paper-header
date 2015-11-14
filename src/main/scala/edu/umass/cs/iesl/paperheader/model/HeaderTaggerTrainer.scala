package edu.umass.cs.iesl.paperheader.model

import java.io._
import java.util.logging.Logger

import cc.factorie._
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.util.HyperparameterMain
import edu.umass.cs.iesl.paperheader._
import edu.umass.cs.iesl.paperheader.load.{LoadGrobid, LoadIESL}

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
    val trainDocs = LoadIESL.fromFilename(opts.trainFile.value).shuffle
    val devDocs = LoadIESL.fromFilename(opts.devFile.value).shuffle
    HeaderDomain.freeze()
    val params = Hyperparams(opts)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new DefaultHeaderTagger(lexicon)
    val result = tagger.train(trainDocs, devDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    result
  }

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value).shuffle
    HeaderDomain.freeze()
    val params = Hyperparams(opts)
    val tagger = new GrobidHeaderTagger
    val result = tagger.train(trainDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    result
  }

  def trainCombined(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value).shuffle
    HeaderDomain.freeze()
    val params = Hyperparams(opts)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new CombinedHeaderTagger(lexicon)
    val result = tagger.train(trainDocs, params)
    if (opts.saveModel.value) {
      tagger.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      log.info(s"serialized model to: ${opts.modelFile.value}")
    }
    result
  }

}
