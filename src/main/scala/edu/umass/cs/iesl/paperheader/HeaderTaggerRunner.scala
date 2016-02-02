package edu.umass.cs.iesl.paperheader

import java.util.logging.Logger
import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.paperheader.model._

/**
 * Created by kate on 1/26/16.
 */

object HeaderTaggerRunner {
  var log: Logger = null

  def inspect(labels: IndexedSeq[HeaderLabel], n: Int): String = {
    labels.take(n).map { label =>
      s"${label.token.string} ${label.target.categoryValue} ${label.categoryValue}"
    }.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val params = new Hyperparams(opts)

    if (Log.log == null) Log(opts.logFile.value)
    log = Log.log
    log.info(opts.unParse.mkString("\n"))
    val docs = HeaderTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value)
    val lexicons = new StaticLexicons()(opts.lexicons.value)
    val tagger = new DefaultHeaderTagger(lexicons, opts.modelFile.value)

    /* TODO uncomment later */
//    val tagger = opts.taggerType.value match {
//      case "grobid" => new GrobidHeaderTagger
//      case "combined" =>
//        val lexicons = new StaticLexicons()(opts.lexicons.value)
//        new CombinedHeaderTagger(lexicons, opts.modelFile.value)
//      case _ =>
//        val lexicons = new StaticLexicons()(opts.lexicons.value)
//        new DefaultHeaderTagger(lexicons, opts.modelFile.value)
//    }


    val labels = docs.flatMap(_.tokens).map(_.attr[HeaderLabel]).toIndexedSeq
    inspect(labels, 50)

    docs.foreach(tagger.process)
    log.info("HeaderTaggerRunner")
    log.info(tagger.evaluation(labels, params).toString())
    log.info("accuracy: " + tagger.objective.accuracy(labels))

    inspect(labels, 50)

    log.info("HEAP DUMP")
    while (true) {

    }
  }
}
