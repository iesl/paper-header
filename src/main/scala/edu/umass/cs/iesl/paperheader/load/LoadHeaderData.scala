package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp
import edu.umass.cs.iesl.paperheader.tagger.{LabeledBioHeaderTag, LabeledBilouHeaderTag}
import scala.collection.mutable
import scala.io.Source
import scala.util.Random._
import scala.xml._

/**
 * Created by kate on 9/25/14.
 */

object LoadCitation {
  def loadFromFile(filename: String): Seq[nlp.Document] = {
    val documents = new mutable.ArrayBuffer[nlp.Document]()
    val docs = Source.fromFile(filename).toString().split("\n")
    println(docs.length)
    for (doc <- docs) {
      println(doc)
    }
    documents.toSeq
  }
}




/**
 * Load data for the HeaderTagger to process.
 */
object LoadTSV {
  // tags present:
//  institution
//  email
//  abstract
//  thesis
//  address
//  tech
//  author
//  title
//  keyword
//  note
//  date

  val tagMap = Map(
  "institution" -> "affiliation",
  "affiliation" -> "affiliation",
  "email" -> "email",
  "abstract" -> "abstract",
  "address" -> "address",
  "author" -> "author",
  "keyword" -> "keyword",
  "date" -> "date",
  "title" -> "title"
  )

  val uniqTags = new scala.collection.mutable.HashSet[String]()


  def fromSource(src: Source, withLabels:Boolean=false, BILOU:Boolean=false, separator: String = "#"): Seq[nlp.Document] = {
    val docs = new mutable.ListBuffer[nlp.Document]()
    val lines = src.getLines().toSeq
    var doc = new nlp.Document("")
    var currLabel = ""
    lines.foreach(line => {
      if (line.startsWith(separator) && doc.tokenCount > 0) {
        docs += doc
        doc = new nlp.Document("")
      } else {
        val parts = line.trim.split("\t")
        if (parts.length >= 2) {
          val labelParts = parts(0).split("-")
          val prefix = labelParts(0)
          val baseLabel = labelParts(1)
          val string = parts(1)
          uniqTags += baseLabel
          if (tagMap.contains(baseLabel)) {
            if (baseLabel != currLabel) {
              currLabel = baseLabel
            }
            val token = new nlp.Token(doc, string)
            // normalize tag e.g. institution --> affiliation
            val newLabel = prefix + "-" + tagMap(baseLabel)
            token.attr += new LabeledBioHeaderTag(token, newLabel)
          }
        }
      }
    })
    // take care of end case
    if (doc.tokenCount > 0) docs += doc
    if (BILOU) convertToBILOU(docs)
    shuffle(docs)
  }

  /** Load documents from filename without storing them in Lines **/
  def loadTSV(filename: String, BILOU:Boolean=false, separator: String = "#"): Seq[nlp.Document] = fromSource(Source.fromFile(filename), BILOU=BILOU, separator=separator)


  def convertToBILOU(documents : mutable.ListBuffer[nlp.Document]) {
    for (doc <- documents) {
      doc.sections.flatMap(_.tokens).foreach(token => {
        val ner = token.attr[LabeledBioHeaderTag]
        var prev : nlp.Token = null
        var next : nlp.Token = null
        if (token.hasPrev) prev = token.prev
        if (token.hasNext) next = token.next
        val newLabel : String = IOBtoBILOU(prev, token, next)
        token.attr += new LabeledBilouHeaderTag(token, newLabel)
      })
    }
  }

  def IOBtoBILOU(prev : nlp.Token, token : nlp.Token,  next : nlp.Token) : String = {
    if(token.attr[LabeledBioHeaderTag].categoryValue == "O") return "O"
    // The major case that needs to be converted is I, which is dealt with here
    val ts = token.attr[LabeledBioHeaderTag].categoryValue.split("-")
    var ps : Array[String] = null
    var ns : Array[String] = null
    if(prev != null)
      ps = splitLabel(prev)
    if(next != null)
      ns = splitLabel(next)

    if(token.attr[LabeledBioHeaderTag].categoryValue.contains("B-")) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      else
        return token.attr[LabeledBioHeaderTag].categoryValue
    }

    if(prev == null || ps(1) != ts(1)) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      return "B-" + ts(1)
    }
    if(next == null || ns(1) != ts(1) || ns(0) == "B")
      return "L-" + ts(1)
    "I-" + ts(1)
  }

  private def splitLabel(token : nlp.Token) : Array[String] = {
    if(token.attr[LabeledBioHeaderTag].categoryValue.contains("-"))
      token.attr[LabeledBioHeaderTag].categoryValue.split("-")
    else
      Array("", "O")
  }


  /**
   *
   * @param filename
   * @param withLabels - if true, then tokens will be labeled with a gold LabeledHeaderTag
   * @return
   */
  def apply(filename:String, withLabels:Boolean = false, separator: String = "\t"): Seq[nlp.Document] = loadTSV(filename, separator=separator)

  def loadDataSets(filename:String, BILOU:Boolean=false): (Seq[nlp.Document], Seq[nlp.Document], Seq[nlp.Document]) = {
    val allDocs = loadTSV(filename, BILOU=BILOU)
    val trainP = (0.7*allDocs.length).floor.toInt
    val trainDocs = allDocs.take(trainP)
    val restDocs = allDocs.drop(trainP)
    val devP = (0.2*restDocs.length).floor.toInt
    val devDocs = restDocs.take(devP)
    val testDocs = restDocs.drop(devP)
    println(s"train=${trainDocs.length}, dev=${devDocs.length}, test=${testDocs.length}")
    (trainDocs, devDocs, testDocs)

  }


}


