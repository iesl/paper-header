package edu.umass.cs.iesl.paperheader.load

import edu.umass.cs.iesl.paperheader.tagger.{LabeledBIOHeaderTag, HeaderLabel}

import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by kate on 10/28/15.
 */
object LoadIESL {

  val DOC_START_PREFIX = "# somedoc"

  def fromFilename(filename: String): Seq[Document] = {
    fromSource(Source.fromFile(filename))
  }

  def fromSource(src: Source): Seq[Document] = {
    val lines = src.getLines()
    val buff = new ArrayBuffer[Document]()
    var doc = new Document("")
    while (lines.hasNext) {
      val line = lines.next()
      if (line.startsWith(DOC_START_PREFIX)) {
        // found a new document
        buff += doc
        doc = new Document("")
      } else {
        val parts = line.split("\t")
        if (parts.length == 2) {
          val label = parts.head
          val string = parts.last
          val token = new Token(doc, string)
          token.attr += new LabeledBIOHeaderTag(token,label)
        }
      }
    }
    buff.toSeq
  }

}
