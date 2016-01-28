package edu.umass.cs.iesl.paperheader

import java.util.logging.Logger
import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.paperheader.model._
/**
 * Created by kate on 1/26/16.
 */
object HeaderTaggerRunner {
  var log: Logger = null
  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val params = new Hyperparams(opts)
    Log(opts.logFile.value)
    log = Log.log
    log.info(opts.unParse.mkString("\n"))
    val docs = HeaderTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value).take(20)
    val tagger = opts.taggerType.value match {
      case "grobid" => new GrobidHeaderTagger
      case "combined" =>
        val lexicons = new StaticLexicons()(opts.lexicons.value)
        new CombinedHeaderTagger(lexicons, opts.modelFile.value)
      case _ =>
        val lexicons = new StaticLexicons()(opts.lexicons.value)
        new DefaultHeaderTagger(lexicons, opts.modelFile.value)
    }

    val labels = docs.flatMap(_.tokens).map(_.attr[HeaderLabel]).toIndexedSeq
    log.info("" + tagger.model.parameters.keys)
    log.info("" + tagger.model.parameters.tensors)
    labels.foreach(_.setRandomly)
    docs.foreach(tagger.process)
    println(tagger.evaluation(labels, params))
  }
}
