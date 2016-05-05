package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import edu.umass.cs.iesl.paperheader.model.DefaultHeaderTagger

/**
 * Created by kate on 10/28/15.
 */
object LoadModel {
  def fromFilename(filename: String): DefaultHeaderTagger = {
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    new DefaultHeaderTagger(None, lexicon, filename)
  }
}
