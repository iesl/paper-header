package edu.umass.cs.iesl.paperheader

import cc.factorie.variable._
import cc.factorie.app.nlp
import cc.factorie.app.nlp.load._
import scala.io.Source
import scala.xml._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
//import org.xml.sax.{SAXException, SAXParseException}



/**
 * Created by kate on 9/25/14.
 */


// TODO tokenize on both whitespace and punctuation (e.g. want "Hello," --> "Hello", ",")


object HeaderFieldDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
    "pubnum", "date", "note", "intro", "address", "title", "phone"
  )
  freeze()
}

class HeaderField(val token:nlp.Token, initialCategory:String) extends CategoricalVariable(initialCategory) { def domain = HeaderFieldDomain }
class LabeledHeaderField(token:nlp.Token, initialCategory:String) extends HeaderField(token, initialCategory) with CategoricalLabeling[String]

object LoadHeaderSGML extends Load {

  val whitespace = "\\s".r
  val docStartMatcher = "<NEW_HEADER>".r
  val tagMatcher = new Regex("<(\\S+)>", "tag")
  val tags: Set[String] = List(
    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
    "pubnum", "date", "note", "intro", "address", "title", "phone").toSet

  def mkDoc(lines:Seq[String]): nlp.Document = {
    val doc = new nlp.Document("")
    val string = lines.filter(l => l.length > 0).mkString("\n")
//    println("string:\n")
//    println(string)
//    print("")
    try {
      val xmlString = XML.loadString(string)
      tags.foreach(tag => {
        val text = (xmlString \ tag).text
        val words = whitespace.split(text)
        words.foreach(word => {
          val token = new nlp.Token(doc, word)
          token.attr += new LabeledHeaderField(token, tag)
        })
      })
    } catch {
      case _ => println("FIXME: Unknown exception: should be handling SAXParseException because of possibly malformed XML")
    }
    doc
  }

  def fromSource(source:Source): Seq[nlp.Document] = {
    val docs = new ArrayBuffer[nlp.Document]()
    val lines = source.getLines().toSeq
    val lineSeq = new ArrayBuffer[String]()
    lineSeq += lines(0)
    lines.drop(1).foreach(line => {
      val m = docStartMatcher.findFirstIn(line)
      if (m.nonEmpty) {
        //found a new doc; add the one we've built up to docs and clear the lineseq
        lineSeq += "</NEW_HEADER>" //otherwise we get an xml error
        docs += mkDoc(lineSeq)
        lineSeq.clear()
        lineSeq += line
      } else {
        lineSeq += line
      }
    })
    docs
  }
  override def fromFile(file:java.io.File): Seq[nlp.Document] = fromSource(Source.fromFile(file))
}

object LoadTester {
  def main(args:Array[String]): Unit = {
    val path = "/iesl/canvas/ksilvers/paperheader/data/tagged_headers.txt"
    val docs = LoadHeaderSGML.fromFilename(path)
    assert(docs.length >= 2)
    println(s"got ${docs.length} docs")
    docs.take(2).foreach(doc => {
      val tokens = doc.tokens.toSeq
      tokens.foreach(token => {
        println(s"${token.string}\t${token.attr[LabeledHeaderField].categoryValue}")
      })
      println("")
    })
  }
}
