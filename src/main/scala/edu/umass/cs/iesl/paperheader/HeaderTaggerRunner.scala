package edu.umass.cs.iesl.paperheader

import java.util.logging.Logger
import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.paperheader.model._
/**
 * Created by kate on 1/26/16.
 */
object HeaderTaggerRunner {
  private val log = Logger.getLogger(getClass.getName)
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val params = new Hyperparams(opts)
    log.info(opts.unParse.mkString("\n"))
    val docs = HeaderTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value)
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
    docs.foreach(tagger.process)
    println(tagger.evaluation(labels, params))
  }
}
