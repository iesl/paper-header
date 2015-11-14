package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.{Document, Token}
import edu.umass.cs.iesl.paperheader.model.GoldHeaderTag

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/**
 * Created by kate on 11/14/15.
 */
object LoadIESL {

  def fromSource(src: Source): Seq[Document] = {
    val lines = src.getLines()
    val buff = new ArrayBuffer[Document]()
    var count = 0
    var doc = new Document(s"Header-$count")
    while (lines.hasNext) {
      val line = lines.next()
      if (line.length > 0) {
        if (line.startsWith("# somedoc")) {
          if (doc.tokenCount > 0) { buff += doc; count += 1 }
          doc = new Document(s"Header-$count")
        } else {
          val parts = line.split("\t")
          assert(parts.length == 2, s"bad line? $line")
          val token = new Token(doc, parts.last)
          val label = new TempLabel(token, parts.head)
          token.attr += label
        }
      }
    }
    if (doc.tokenCount > 0) buff += doc
    buff.foreach(iob2bilou)
    buff.toSeq
  }

  def fromFilename(filename: String): Seq[Document] = fromSource(Source.fromFile(filename))

}
