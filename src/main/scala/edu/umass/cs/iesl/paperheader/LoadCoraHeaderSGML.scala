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



object LoadCoraHeaderSGML extends Load {

  val whitespace = "\\s".r
  val docStartMatcher = "<NEW_HEADER>".r
  val tagMatcher = new Regex("<(\\S+)>", "tag")
  val tags: Set[String] = List(
    "author", "email", "affiliation", "degree", "abstract", "keyword", "web",
    "pubnum", "date", "note", "intro", "address", "title", "phone").toSet

  //FIXME there's probably a way to simplify this
  //FIXME catch/handle actual exceptions
  def mkDoc(lines:Seq[String]): nlp.Document = {
    val doc = new nlp.Document("")
    val string = lines.filter(l => l.length > 0).mkString("\n")
    try {
      val xmlString = XML.loadString(string)
      tags.foreach(tag => {
        val text = (xmlString \ tag).text
        val lines = "\\n".r.split(text)
        lines.foreach(line => {
          val words = whitespace.split(line)
          for (i <- 0 until words.length) {
            val w = words(i)
            val token = new nlp.Token(doc, w)
            if (i == 0) token.attr += new LabeledBilouHeaderTag(token, "B-"+tag)
            else if (i == words.length-1) token.attr += new LabeledBilouHeaderTag(token, "L-"+tag)
            else if (words.length == 1) token.attr += new LabeledBilouHeaderTag(token, "U-"+tag)
            else token.attr += new LabeledBilouHeaderTag(token, "I-"+tag)
//            if (i == 0) token.attr += new LabeledHeaderTag(token, "B-"+tag)
//            else if (i == words.length-1) token.attr += new LabeledHeaderTag(token, "L-"+tag)
//            else if (words.length == 1) token.attr += new LabeledHeaderTag(token, "U-"+tag)
//            else token.attr += new LabeledHeaderTag(token, "I-"+tag)
          }
          doc.appendString(" ")
//          val newline = new nlp.Token(doc, "\n")
//          newline.attr += new LabeledHeaderTag(newline, "O")
        })
      })
    } catch {
      case _ : Throwable => //println("FIXME: Unknown exception: should be handling SAXParseException because of possibly malformed XML")
    }
    doc.asSection.chainFreeze()
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

  override def fromFile(file:java.io.File): Seq[nlp.Document] = {
    val docs = fromSource(Source.fromFile(file))
    println(s"Loaded ${docs.length} docs from ${file.getName}")
    docs
  }
}

object LoadTester {
  def main(args:Array[String]): Unit = {
    val path = "/iesl/canvas/ksilvers/paperheader/data/tagged_headers.txt"
    val docs = LoadCoraHeaderSGML.fromFilename(path)
    assert(docs.length >= 2)
    println(s"got ${docs.length} docs")

//    docs.foreach(doc => {
//      val sections:Seq[LabeledHeaderSection] = doc.attr.all.filter(v => v.isInstanceOf[LabeledHeaderSection])
//      if (sections.length > 0){
//        println(s"# sections: ${sections.length}")
//        sections.foreach(s => {
//          println(s)
//          val tokens = s.tokenize()
//          if (tokens.length >= 5) tokens.take(5).foreach(t => println("   " + t.string))
//        })
//      }
//    })
  }
}
