package edu.umass.cs.iesl.paperheader

import java.util.logging.Logger

import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.paperheader.model._

/**
 * Created by kate on 1/26/16.
 */

object HeaderTaggerRunner {

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
    val logOpt = Some(opts.logFile.value)
    val tagger = opts.taggerType.value match {
      case "grobid" => new GrobidHeaderTagger(logOpt)
      case "combined" =>
        val lexicons = new StaticLexicons()(opts.lexicons.value)
        new CombinedHeaderTagger(logOpt, lexicons, opts.modelFile.value)
      case _ =>
        val lexicons = new StaticLexicons()(opts.lexicons.value)
        new DefaultHeaderTagger(logOpt, lexicons, opts.modelFile.value)
    }
    val docs = HeaderTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value)
    val labels = docs.flatMap(_.tokens).map(_.attr[HeaderLabel]).toIndexedSeq
    docs.foreach(tagger.process)
    tagger.log.info("HeaderTaggerRunner")
    tagger.log.info(tagger.evaluation(labels, params.segmentScheme).toString())
    tagger.log.info("accuracy: " + tagger.objective.accuracy(labels))
  }

}
