package edu.umass.cs.iesl.paperheader.model

import java.io._

import cc.factorie.app.nlp.{Document, Token}
import edu.umass.cs.iesl.paperheader.load.PreFeatures

/**
 * Created by kate on 1/26/16.
 */
class GrobidHeaderTagger extends AbstractHeaderTagger {

  def this(url: java.net.URL) = {
    this()
    deserialize(url.openConnection().getInputStream)
    log.info(s"deserialized model from ${url.getPath}")
  }

  def this(path: String) = this(new File(path).toURL())

  override def addFeatures(document: Document): Unit = {
    val vf = (t: Token) => t.attr[HeaderFeatures]
    document.tokens.foreach { token =>
      val features = new HeaderFeatures(token)
      features ++= token.attr[PreFeatures].features
      token.attr += features
    }
  }

}
