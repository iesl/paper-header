package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.{Sentence, Document, Token}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author Kate Silverstein 
 *         created on 4/22/15
 */

class PreFeatures(val features: Array[String], val token: Token)

object LoadGrobid {
  def fromFilename(filename: String, withFeatures: Boolean = true): Seq[Document] = {
    import edu.umass.cs.iesl.paperheader.tagger.{LabelDomain, HeaderLabel}
    println(s"Loading data from $filename ...")
    val whitespace = "\\s+".r
    val buff = new ArrayBuffer[Document]()
    var currDoc = new Document("")
    var currSent = new Sentence(currDoc)
    val lines = Source.fromFile(filename).getLines()
    var tokenCount = 0
    var docCount = 0
//    var state = 0
    while (lines.hasNext) {
      val line = lines.next()
      val parts = whitespace.split(line)
      if (parts.length > 1) {
        val label = {
          val l = parts.last.dropRight(1)
          if (l.startsWith("I-<")) {
            val ll = l.drop(3)
            "B-" + ll
          } else {
            val ll = l.drop(1)
            "I-" + ll
          }
        }
//        val label = parts.last
//        state match {
//          case 0 =>
//        }


        val string = parts.head
        val features = parts.dropRight(1)
        val token = new Token(currSent, string)
        if (withFeatures) token.attr += new PreFeatures(features, token) //put in PreFeatures so we can freeze CitationFeaturesDomain after loading training / before loading dev
        token.attr += new HeaderLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(label)) label else "O", token)
        tokenCount += 1
      } else {
        if (currSent.length > 0) currDoc.appendString("")
        if (currDoc.tokenCount > 0) {
          buff += currDoc
          currDoc = new Document("")
          currSent = new Sentence(currDoc)
          docCount += 1
        }
      }
    }
    println(s"Loaded $docCount docs with $tokenCount tokens from file $filename.")
    buff
  }
}
