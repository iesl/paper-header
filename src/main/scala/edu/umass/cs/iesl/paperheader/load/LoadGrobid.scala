package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.{Document, Token}
import edu.umass.cs.iesl.paperheader.model.GrobidFeatures

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import java.util.logging.Logger

/**
 * @author Kate Silverstein 
 *         created on 4/22/15
 */

object LoadGrobid {

  private val log = Logger.getLogger(getClass.getName)

  val brackets = "[<>]".r

  def fromSource(src: Source): Seq[Document] = {
    val lines = src.getLines()
    val buff = new ArrayBuffer[Document]()
    var count = 0
    var doc = new Document(s"Grobid-$count")
    while (lines.hasNext) {
      val line = lines.next()
      if (line.length == 0) {
        if (doc.tokenCount > 0) { buff += doc; count += 1 }
        doc = new Document(s"Grobid-$count")
      } else {
        val parts = line.split(" ")
        assert(parts.length == 33, s"bad line? $line")
        val token = new Token(doc, parts.head)
        val labelStr = parts.last
        val labelStrClean = {
          val base = if (labelStr.startsWith("I-")) labelStr.split("-").last else labelStr
          val prefix = if (labelStr.startsWith("I-")) labelStr.split("-").head + "-" else ""
          prefix + brackets.replaceAllIn(base, "")
        }
        val label = new TempLabel(token, labelStrClean)
        token.attr += label
        val features = parts.dropRight(1) //parts.slice(1, parts.length - 1)
        token.attr += GrobidFeatures(features.toArray, token)
      }
    }
    if (doc.tokenCount > 0) buff += doc
    buff.foreach(iob2bilou)
    buff.toSeq
  }

  def fromFilename(filename: String): Seq[Document] = fromSource(Source.fromFile(filename))

}
