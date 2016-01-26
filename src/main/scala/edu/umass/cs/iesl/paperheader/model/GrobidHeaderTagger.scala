package edu.umass.cs.iesl.paperheader.model

import cc.factorie.app.nlp.{Token, Document}
import edu.umass.cs.iesl.paperheader.load.PreFeatures

/**
 * Created by kate on 1/26/16.
 */
class GrobidHeaderTagger extends AbstractHeaderTagger {
  override def addFeatures(document: Document): Unit = {
    val vf = (t: Token) => t.attr[HeaderFeatures]
    document.tokens.foreach { token =>
      val features = new HeaderFeatures(token)
      features ++= token.attr[PreFeatures].features
      token.attr += features
    }
  }
}
