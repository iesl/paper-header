package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp._
import scala.io.Source
import scala.collection.mutable

/**
 * Created by kate on 9/25/14.
 */


/**
 * Load data for the HeaderTagger to process.
 *
 * Expects one big file with at least 4 tab-separated columns (5 columns if withLabels = true), which are:
 *
 * (BIO-label) string xpos ypos fontsize
 *
 */
object LoadTSV {
  val tagSet = Seq(
    //    "O",
    "author",
    "institution",
    "title",
//    "keyword",
    "date",
    "email",
    "address",
    "abstract"
//    "note"
  ).toSet

  /**
   * Load documents from a tab-separated file with columns: BIO_gold_label, token_string, y_position, x_position, font_size
   * e.g. I-institution	Yale	2406	4923	-1
   * See config/tagset.config for the list of valid header tags
   * @param filename tab-separated file
   * @param tags a list of tags to use (probably don't change the default value)
   * @param firstLineBlank is the first line of the file blank? (probably don't change the default value)
   * @return a Seq of FACTORIE documents where each token has a LabeledBioHeaderTag and each document as a LineBuffer
   */
  def loadTSVWithFormatInfo(filename: String, tags: Set[String] = tagSet, firstLineBlank: Boolean = false): Seq[Document] = {
    val docs = new mutable.ListBuffer[Document]()
    val lines = if (firstLineBlank) Source.fromFile(filename).getLines().toSeq.drop(1) else Source.fromFile(filename).getLines().toSeq
    assert(lines(0).startsWith("#"), "invalid first line: " + lines(0))
    var doc = new Document("")
    doc.attr += new LineBuffer(doc)
    var currLine = new mutable.ListBuffer[Array[String]]()
    var currYPos: Int = 0
    lines.drop(1).foreach(line => {
      if (line.startsWith("#") && doc.tokenCount > 0) {
        docs += doc
        doc = new Document("")
        doc.attr += new LineBuffer(doc)
      } else if (line.length > 0) {
        val parts = line.trim.split("\t")
        val y = parts(3).toInt
        if (y == currYPos) {
          currLine += parts
        } else {
          //found a new line
          val tokens = currLine.filter(l => tags.contains(l(0).substring(2))).map(l => {
            assert(l.length == 5)
            val label = l(0) //if (l(0) != "O" && !tags.contains(l(0).substring(2))) "O" else l(0)
            val string = l(1)
            val token = new Token(doc, string)
            token.attr += new LabeledBioHeaderTag(token, label)
            val y = currYPos //(l(3).toDouble / 10.0).floor.toInt
            val x = (l(2).toDouble / 10.0).floor.toInt
            val fontSize = if (l(4).toInt == -1) 10 else l(4).toInt
            token.attr += new FormatInfo(token, x, y, fontSize)
            token
          })
          doc.attr[LineBuffer] += new Line(tokens, currYPos)
          // second, update currYPos and currLine, then add this line to currLine
          currYPos = y
          currLine.clear()
          currLine += parts
        }
      }
    })
    //take care of end case (there will be one doc left over)
    if (currLine.length > 0) {
      val tokens = currLine.map(l => {
        val token = new Token(doc, l(1))
        token.attr += new LabeledBioHeaderTag(token, l(0))
        val y = currYPos //(l(3).toDouble / 10.0).floor.toInt
        val x = (l(2).toDouble / 10.0).floor.toInt
        val fontSize = if (l(4).toInt == -1) 10 else l(4).toInt
        token.attr += new FormatInfo(token, x, y, fontSize)
        token
      })
      doc.attr[LineBuffer] += new Line(tokens, currYPos)
    }
    if (doc.tokenCount > 0) docs += doc
    docs
  }

  /** Load documents from filename without storing them in Lines **/
  def loadTSV(filename: String): Seq[Document] = {
    val docs = new mutable.ListBuffer[Document]()
//    val lines = Source.fromFile(filename).getLines().toSeq.drop(1)
//    assert(lines(0).startsWith("#"), s"first line should start with '#', instead starts with '${lines(0)(0)}'!")
    val lines1 = Source.fromFile(filename).getLines().toSeq
    val firstLine = lines1(0)
    val lines: Seq[String] = {
      if (firstLine.length == 0) lines1.drop(2)
      else lines1
    }
    print(s"first line: ${lines(0)}")
    var doc = new Document("")
    lines.drop(1).foreach(line => {
      if (line.startsWith("#")) {
        docs += doc
        doc = new Document("")
      } else {
        val parts = line.trim.split("\t")
        if (parts.length >= 2) {
          val label = parts(0)
          val string = parts(1)
          if (tagSet.contains(label.substring(2))) {
            val token = new Token(doc, string)
            token.attr += new LabeledBioHeaderTag(token, label)
          }
        }
      }
    })
    // take care of end case
    if (doc.tokenCount > 0) docs += doc
    docs
  }
  /**
   *
   * @param filename
   * @param withLabels - if true, then tokens will be labeled with a gold LabeledHeaderTag
   * @return
   */
  def apply(filename:String, withLabels:Boolean = false, withFormatting: Boolean = false): Seq[Document] = {
    if (withFormatting) loadTSVWithFormatInfo(filename)
    else loadTSV(filename)
  }
}


