package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp.{Sentence, Document, Token}
import edu.umass.cs.iesl.paperheader.model.{HeaderLabelDomain, HeaderLabel}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author Kate Silverstein 
 *         created on 4/22/15
 */

class PreFeatures(val features: Array[String], val token: Token)

object LoadGrobid {

  private val log = edu.umass.cs.iesl.paperheader.Log.log
  val whitespace = "\\s+".r

  def fixLastLabel(lastLabel: HeaderLabel) =
    if(lastLabel.categoryValue(0) == 'B') lastLabel.set(HeaderLabelDomain.index("U" + lastLabel.categoryValue.drop(1)))(null)
    else lastLabel.set(HeaderLabelDomain.index("L" + lastLabel.categoryValue.drop(1)))(null)

  def fromFilename(filename: String, withFeatures: Boolean = true, bilou: Boolean = false): Seq[Document] = {
    println(s"Loading data from $filename ...")
    val buff = new ArrayBuffer[Document]()
    var currDoc = new Document("")
    var currSent = new Sentence(currDoc)
    val lines = Source.fromFile(filename).getLines()
    var tokenCount = 0
    var docCount = 0
    var state = 0
    var lastLabel: HeaderLabel = null
    while (lines.hasNext) {
      val line = lines.next()
      val parts = whitespace.split(line)
      if (parts.length > 1) {
        val label = {
          val l = parts.last.dropRight(1)
          if (bilou) {
            val label = parts.last
            label(0) match {
              case 'I' =>
                if(lastLabel != null) fixLastLabel(lastLabel)
                "B-" + l.drop(3)
              case '<' =>
                if(lastLabel != null && label(1) != lastLabel.categoryValue(2)){
                  fixLastLabel(lastLabel)
                  "B-" + l.drop(1)
                }
                else "I-" + l.drop(1)
            }
          }
          else {
            if (l.startsWith("I-<")) {
              val ll = l.drop(3)
              "B-" + ll
            } else {
              val ll = l.drop(1)
              "I-" + ll
            }
          }
        }

        val string = parts.head
        val features = parts.dropRight(1).zipWithIndex.map{case(f, i) => "G@" + i + "=" + f}
        val token = new Token(currSent, string)
        if (withFeatures) token.attr += new PreFeatures(features, token) //put in PreFeatures so we can freeze CitationFeaturesDomain after loading training / before loading dev
        val hLab = new HeaderLabel(if (!HeaderLabelDomain.frozen || HeaderLabelDomain.categories.contains(label)) label else "O", token)
        token.attr += hLab
        lastLabel = hLab
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
    fixLastLabel(lastLabel)
    println(s"Loaded $docCount docs with $tokenCount tokens from file $filename.")
    if (log != null) {
      log.info(s"Loaded $docCount docs with $tokenCount tokens from file $filename.")
    }

    //println(buff.flatMap(_.tokens).map(t => t.string + "\t" + t.attr[HeaderLabel].categoryValue).mkString("\n"))
    buff
  }
}
