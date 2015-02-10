package edu.umass.cs.iesl.paperheader.crf

import cc.factorie.variable._
import cc.factorie.app.nlp
import cc.factorie.app.nlp.load._
import scala.io.Source
import scala.xml

import scala.util.matching.Regex
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
  /** Load documents from filename, grouping tokens by y-position (lines) **/
  def loadTSV(filename: String): Seq[nlp.Document] = {
    val docs = new mutable.ListBuffer[nlp.Document]()
    val lines = Source.fromFile(filename).getLines().toSeq.drop(1)
    assert(lines(0).startsWith("#"), s"first line should start with '#', instead starts with '${lines(0)(0)}'!")
    var doc = new nlp.Document("")
    doc.attr += new LineBuffer(doc)
    var currLine = new mutable.ListBuffer[Array[String]]()
    var currYPos: Int = 0
    lines.drop(1).foreach(line => {
      if (line.startsWith("#") && doc.tokenCount > 0) {
        docs += doc
        doc = new nlp.Document("")
        doc.attr += new LineBuffer(doc)
      } else {
        val parts = line.trim.split("\t")
        if (parts.length == 5) {
          val Array(_, _, _, ys, _) = parts
          val y = ys.toInt
          if (y == currYPos) currLine += parts
          else {
            //found a new line
            val tokens = currLine.map(l => {
              val token = new nlp.Token(doc, l(1))
              //ignore "tech", "thesis", "note" for now
              var label = l(0)
              if (label != "O") {
                val base = label.substring(2)
                if (base == "tech" || base == "thesis" || base == "note") label = "O"
              }
              token.attr += new LabeledBioHeaderTag(token, label)
              //TODO FormatInfo
              token
            })
            doc.attr[LineBuffer] += new Line(tokens, currYPos)
            currYPos = y
            currLine.clear()
          }
        }
      }
    })
    //take care of end case (there will be one doc left over)
    if (currLine.length > 0) {
      val tokens = currLine.map(l => { val token = new nlp.Token(doc, l(1)); token.attr += new LabeledBioHeaderTag(token, l(0)); token})
      doc.attr[LineBuffer] += new Line(tokens, currYPos)
    }
    if (doc.tokenCount > 0) docs += doc
    docs
  }

  /** Load documents from filename without storing them in Lines **/
  def loadTSVSimple(filename: String): Seq[nlp.Document] = {
    val docs = new mutable.ListBuffer[nlp.Document]()
    val lines = Source.fromFile(filename).getLines().toSeq.drop(1)
    assert(lines(0).startsWith("#"), s"first line should start with '#', instead starts with '${lines(0)(0)}'!")
    var doc = new nlp.Document("")
    lines.drop(1).foreach(line => {
      if (line.startsWith("#")) {
        docs += doc
        doc = new nlp.Document("")
      } else {
        val parts = line.trim.split("\t")
        if (parts.length == 5) {
          val Array(lab, string, _, _, _) = parts
          //ignore "tech", "thesis", "note" for now
          var label = lab
          if (label != "O") {
            val base = label.substring(2)
            if (base == "tech" || base == "thesis" || base == "note") label = "O"
          }
          val token = new nlp.Token(doc, string)
          token.attr += new LabeledBioHeaderTag(token, label)
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
  def apply(filename:String, withLabels:Boolean = false): Seq[nlp.Document] = loadTSV(filename)
}


