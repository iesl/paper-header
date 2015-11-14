package edu.umass.cs.iesl.paperheader

import cc.factorie.app.nlp.{Token, Document}
import edu.umass.cs.iesl.paperheader.model.GoldHeaderTag

/**
 * Created by kate on 11/14/15.
 */
package object load {

  case class TempLabel(token: Token, iobLabel: String) {
    def prefix: String = if (iobLabel.equals("O")) "O" else iobLabel.split("-").head
    def category: String = if (iobLabel.equals("O")) "O" else iobLabel.split("-").last
  }

  def iob2bilou(doc: Document): Unit = {
    val tokens = doc.tokens.toSeq
    val n = tokens.length
    var i = 0
    var prevCat = "O"
    var chunkLen = 0
    while (i < n) {
      val token = tokens(i)
      val tag = token.attr[TempLabel]
      val cat = tag.category
      if (!cat.equals(prevCat)) {
        if (i > 0) {
          val prevToken = tokens(i-1)
          prevToken.attr.remove[GoldHeaderTag]
          if (chunkLen == 1) prevToken.attr += new GoldHeaderTag(prevToken, "U-" + prevCat)
          else prevToken.attr += new GoldHeaderTag(prevToken, "L-" + prevCat)
        }
        token.attr += new GoldHeaderTag(token, "B-" + cat)
        prevCat = cat
        chunkLen = 1
      } else {
        token.attr += new GoldHeaderTag(token, "I-" + cat)
        chunkLen += 1
      }
      token.attr.remove[TempLabel]
      i += 1
    }
    val lastToken = tokens.last
    val lastLabel = lastToken.attr[GoldHeaderTag].categoryValue
    if (!lastLabel.equals("O")) {
      val lastCat = lastLabel.split("-").last
      lastToken.attr.remove[GoldHeaderTag]
      if (chunkLen == 1) lastToken.attr += new GoldHeaderTag(lastToken, "U-" + lastCat)
      else lastToken.attr += new GoldHeaderTag(lastToken, "L-" + lastCat)
    }
  }

}
