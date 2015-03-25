package edu.umass.cs.iesl.paperheader.crf

/**
 * @author Kate Silverstein 
 *         created on 3/24/15
 */

import scala.xml._
import cc.factorie.app.nlp
import scala.collection.mutable.ArrayBuffer

/**
  Grobid Data Stats
  total docs: 1159
  total tokens: 206248
  label count count/total
  date	1366	0.006623094526977231
  keyword	5487	0.026603894340793608
  title	11545	0.055976300376246076
  abstract	162376	0.7872852100384004
  email	2659	0.01289224622784221
  author	9590	0.04649742058104806
  institution	13225	0.06412183390869244
  */

object LoadGrobid {
  def fromDirectory(dirname: String): Seq[nlp.Document] = new java.io.File(dirname).listFiles.map(_.getPath).map(f => fromFilename(f))
  def fromFilename(filename: String): nlp.Document = {
    val doc = new nlp.Document("")
    val xml = XML.loadFile(filename)
    val title = xml \\ "docTitle" \ "titlePart"
    val authors = xml \\ "byline" \ "docAuthor"
    val affiliation = xml \\ "byline" \ "affiliation"
    val address = xml \ "address"
    val abstr = (xml \\ "div").filter(div => (div \\ "@type").text == "abstract")
    val keywords = xml \\ "keywords"
    val email = xml \\ "email"
    val date = xml \\ "date"
    List("title", "author", "institution", "address", "abstract", "keyword", "email", "date")
      .zip(List(title, authors, affiliation, address, abstr, keywords, email, date))
      .foreach({ case (label, elems) =>
      if (elems.text.length > 0) tokenizeAndTagSegment(elems.text, label, doc)
    })
    //    tokenizeAndTagSegment((xml \\ "docTitle" \ "titlePart").text, "title", doc)
    //    tokenizeAndTagSegment((xml \\ "byline" \ "docAuthor").text, "author", doc)
    //    tokenizeAndTagSegment((xml \\ "byline" \ "affiliation").text, "institution", doc)
    //    tokenizeAndTagSegment((xml \ "address").text, "address", doc)
    //    tokenizeAndTagSegment((xml \\ "div").filter(div => {
    //      val dtype = (div \\ "@type").text
    //      dtype == "abstract"
    //    }).map(_.text).head, "abstract", doc)
    //    tokenizeAndTagSegment((xml \\ "keywords").text, "keyword", doc)
    //    tokenizeAndTagSegment((xml \\ "email").text, "email", doc)
    //    tokenizeAndTagSegment((xml \\ "date").text, "date", doc)
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
    import scala.collection.mutable.HashMap
    val docs = fromDirectory("/home/kate/research/another-ph-clean/paper-header/grobid-full")
    println(s"total docs: ${docs.length}")
    val labelMap = new HashMap[String, Int]()
    val tokens = docs.flatMap(_.sections.flatMap(_.tokens))
    tokens.foreach(t => {
      val label = t.attr[LabeledBioHeaderTag].categoryValue.split("-")(1)
      if (!labelMap.contains(label)) labelMap(label) = 1
      else labelMap(label) += 1
    })
    println(s"total tokens: ${tokens.length}")
    labelMap.keySet.foreach(k => println(s"$k\t${labelMap(k)}\t${labelMap(k).toDouble / tokens.length.toDouble}"))
  }
}
