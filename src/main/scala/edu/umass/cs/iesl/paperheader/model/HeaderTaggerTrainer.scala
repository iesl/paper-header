package edu.umass.cs.iesl.paperheader.model

import java.io.FileOutputStream

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.paperheader._
import edu.umass.cs.iesl.paperheader.load.{LoadTSV, LoadGrobid}
import cc.factorie.util._
import java.util.logging.Logger

/**
 * Created by kate on 1/26/16.
 */
object HeaderTaggerTrainer extends HyperparameterMain {
  var log: Logger = null
  def evaluateParameters(args: Array[String]): Double = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    Log(opts.logFile.value)
    log = Log.log
    log.info(opts.unParse.mkString("\n"))
    opts.taggerType.value match {
      case "grobid" => trainGrobid(opts)
      case "combined" => trainCombined(opts)
      case _ => trainDefault(opts)
    }
  }

  def testTagger(inputFilename: String, tagger: DefaultHeaderTagger, params: Hyperparams, extra: String)(implicit random: scala.util.Random): Unit = {
    val docs = loadDocs(inputFilename, "grobid")
    val labels = docs.flatMap(_.tokens).map(_.attr[HeaderLabel]).toIndexedSeq
    labels.foreach(_.setRandomly)
    docs.foreach(tagger.process)
    log.info(extra)
    log.info(tagger.evaluation(labels, params).toString())
  }

  def trainDefault(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val trainDocs = loadDocs(opts.trainFile.value, opts.dataType.value).take(100) //TODO change me back
    val devDocs = if (opts.devFile.wasInvoked) loadDocs(opts.devFile.value, opts.dataType.value) else Seq()
    val lexicons = new StaticLexicons()(opts.lexicons.value)
    val tagger = new DefaultHeaderTagger(lexicons)
    val result = tagger.train(trainDocs.take(100), devDocs.take(20), params)
    if (opts.saveModel.value) {
      log.info(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
      log.info("" + tagger.model.parameters.keys)
      log.info("" + tagger.model.parameters.tensors)
    }

    /* TODO: remove me later */
    /* test performance now vs. deserializing */
    testTagger(opts.devFile.value, tagger, params, "test set (before deserialization)")
    val taggerReloaded = new DefaultHeaderTagger(lexicons, opts.modelFile.value)
    testTagger(opts.devFile.value, taggerReloaded, params, "test set (after deserialization)")

    log.info("\n\n")
    log.info("passing to HeaderTaggerRunner")
    val args = Array(
      s"--test-file=${opts.devFile.value}",
      s"--model-file=${opts.modelFile.value}",
      s"--log-file=${opts.logFile.value}",
      "--data-type=grobid",
      "--tagger-type=default"
    )
    edu.umass.cs.iesl.paperheader.HeaderTaggerRunner.main(args)

    result
  }

  def trainGrobid(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = if (opts.devFile.wasInvoked) LoadGrobid.fromFilename(opts.devFile.value) else Seq()
    val tagger = new GrobidHeaderTagger
    val result = tagger.train(trainDocs, devDocs, params)
    if (opts.saveModel.value) {
      log.info(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    result
  }

  def trainCombined(opts: HeaderTaggerOpts): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = if (opts.devFile.wasInvoked) LoadGrobid.fromFilename(opts.devFile.value) else Seq()
    val lexicons = new StaticLexicons()(opts.lexicons.value)
    val tagger = new DefaultHeaderTagger(lexicons)
    val result = tagger.train(trainDocs, devDocs, params)
    if (opts.saveModel.value) {
      log.info(s"serializing model to: ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    result
  }

  /*

  Helpers

   */
  def loadDocs(filename: String, dataType: String): Seq[Document] = {
    dataType match {
      case "grobid" => LoadGrobid.fromFilename(filename)
      case "iesl" => LoadTSV.loadTSV(filename)
      case _ => throw new Exception(s"invalid data type: $dataType")
    }
  }
}


object OptimizeHeaderTagger {
  private val log = Logger.getLogger(getClass.getName)
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    log.info(opts.unParse.mkString("\n"))
    opts.saveModel.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 10))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 10))
    val rate = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-4, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 1))
    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.paperheader.model.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    log.info("Got results: " + result.mkString(" "))
    log.info("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    log.info("Running best configuration...")
    opts.saveModel.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    log.info("Done.")
  }
}