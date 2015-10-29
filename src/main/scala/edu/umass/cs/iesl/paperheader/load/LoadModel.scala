package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import edu.umass.cs.iesl.paperheader.tagger.HeaderTagger

/**
 * Created by kate on 10/28/15.
 */
object LoadModel {
  def fromFilename(filename: String): HeaderTagger = {
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    new HeaderTagger(lexicon, filename)
  }
}
