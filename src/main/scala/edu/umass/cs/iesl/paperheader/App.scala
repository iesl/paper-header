package edu.umass.cs.iesl.paperheader

/**
 * Created by kate on 10/28/15.
 */

import cc.factorie.app.nlp.lexicon.{StaticLexicons,LexiconsProvider}
import edu.umass.cs.iesl.paperheader.tagger._
import edu.umass.cs.iesl.paperheader.load.LoadIESL


object App {
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)

    val modelFile = opts.modelFile.value
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val model = new BIOHeaderTagger(lexicon, modelFile)

    val inputFile = opts.testFile.value
    val docs = LoadIESL.fromFilename(inputFile)
    docs.foreach(model.process)

    for (d <- docs.take(5)) {
      for (t <- d.tokens) {
        println(s"\t${t.string}\t${t.attr[BIOHeaderTag].categoryValue}")
      }
      println("")
    }

  }
}
