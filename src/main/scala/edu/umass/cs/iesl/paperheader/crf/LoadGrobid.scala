package edu.umass.cs.iesl.paperheader.crf

/**
 * @author Kate Silverstein 
 *         created on 3/24/15
 */

import scala.xml._
import cc.factorie.app.nlp
import scala.collection.mutable.ArrayBuffer

object LoadGrobid {
  def fromFilename(filename: String): nlp.Document = {
    val doc = new nlp.Document("")
    val xml = XML.loadFile(filename)
    tokenizeAndTagSegment((xml \\ "docTitle" \ "titlePart").text, "title", doc)
    tokenizeAndTagSegment((xml \\ "byline" \ "docAuthor").text, "author", doc)
    tokenizeAndTagSegment((xml \\ "byline" \ "affiliation").text, "institution", doc)
    tokenizeAndTagSegment((xml \ "address").text, "address", doc)
    tokenizeAndTagSegment((xml \\ "div").filter(div => {
      val dtype = (div \\ "@type").text
      dtype == "abstract"
    }).map(_.text).head, "abstract", doc)
    tokenizeAndTagSegment((xml \\ "keywords").text, "keyword", doc)
    tokenizeAndTagSegment((xml \\ "email").text, "email", doc)
    tokenizeAndTagSegment((xml \\ "date").text, "date", doc)
    doc
  }
  def tokenizeAndTagSegment(seg: String, label: String, doc: nlp.Document): Unit = {
    import cc.factorie.app.nlp.segment.DeterministicTokenizer
    if (seg.length > 0) {
      val tokenizer = new DeterministicTokenizer
      val tmpDoc = new nlp.Document(seg)
      tokenizer.process(tmpDoc)
      val tokens = tmpDoc.sections.flatMap(_.tokens)
      tokens.foreach(t => {
        val tok = new nlp.Token(doc, t.string)
        tok.attr += new LabeledBioHeaderTag(tok, "I-" + label)
      })
    }
  }
  def main(args: Array[String]): Unit = {
    val doc = fromFilename("/home/kate/research/another-ph-clean/paper-header/grobid-subset/header447.tei")
    doc.sections.flatMap(_.tokens).foreach(t => {
      println(s"${t.string} ${t.attr[LabeledBioHeaderTag].categoryValue}")
    })
  }

}
