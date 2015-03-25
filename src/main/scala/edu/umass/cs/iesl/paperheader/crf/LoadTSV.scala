package edu.umass.cs.iesl.paperheader.crf

import cc.factorie.app.nlp._
import scala.io.Source
import scala.collection.mutable.{ListBuffer, HashMap}
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
 * Stats for data/fullpaper-headers.{train, dev, test}
    total docs: 445
    total tokens: 99655
    date	424	0.004254678641312528
    note	10792	0.10829361296472831
    address	3198	0.032090712959711006
    keyword	2041	0.02048065827103507
    thesis	4	4.01384777482314E-5
    title	4466	0.04481461040590035
    abstract	70289	0.7053233656113592
    email	846	0.008489288043750941
    author	3030	0.030404896894285285
    tech	156	0.0015654006321810245
    institution	4409	0.044242637097988056
 *
 */
object LoadTSV {
  /**
   *
   * @param filename
   * @param withLabels - if true, then tokens will be labeled with a gold LabeledHeaderTag
   * @return
   */
  def apply(filename:String, withLabels:Boolean = true) = { new LoadTSV(withLabels=withLabels).fromFilename(filename, separator="#") }
  def main(args: Array[String]): Unit = {
    import scala.collection.mutable.HashMap
    val loader = new LoadTSV(withLabels=true)
    val td = loader.fromFilename("/home/kate/research/another-ph-clean/paper-header/data/fullpaper-headers.train")
    val dd = loader.fromFilename("/home/kate/research/another-ph-clean/paper-header/data/fullpaper-headers.dev")
    val testd = loader.fromFilename("/home/kate/research/another-ph-clean/paper-header/data/fullpaper-headers.test")
    val docs = td ++ dd ++ testd
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

class LoadTSV(val withLabels: Boolean = true) {
  def fromFilename(filename: String, separator: String = "#", withFormatting: Boolean = false): Seq[Document] = {
    if (withFormatting) fromSourceWithFormatting(Source.fromFile(filename), separator)
    else fromSource(Source.fromFile(filename), separator)
  }
  def fromSource(source: Source, separator: String): Seq[Document] = {
    val docs = new ListBuffer[Document]()
    var doc = new Document("")
    source.getLines().filter(_.length > 0).foreach(line => {
      if (line.startsWith("#")) {
        //if (doc.tokenCount > 0) { docs += doc; doc = new Document("") }
        docs += doc
        doc = new Document("")
      } else {
        val parts = line.trim.split("\t")
        if (parts.length >= 2) { // token has label in column 0
          val label = parts(0)
          val string = parts(1)
          val token = new Token(doc, string)
          token.attr += new LabeledBioHeaderTag(token, label)
        } else if (parts.length == 1) { // token has no label, and line is not blank
          val _ = new Token(doc, parts(0))
        }
      }
    })
    // take care of end case -- there is still a document leftover
    if (doc.tokenCount > 0) docs += doc
    docs.filter(_.tokenCount > 0).toSeq
  }

  def fromSourceWithFormatting(source:Source, separator: String): Seq[Document] = {
    println("LOADTSV: withLabels = " + withLabels)
    val docs = new ListBuffer[Document]()
    val l = source.getLines().toSeq
    val firstLineLen = l(0).length
    val lines = if (firstLineLen == 0) l.drop(1) else l
    val docName = lines(0)
    assert(lines(0).startsWith(separator), lines(0))
    var currDoc = new Document("").setName(lines(0))
    for (line <- lines) {
      if (line.startsWith(separator)) {
        //found a new document
        if (currDoc.tokenCount > 0) {
          val table = new HashMap[Int, ListBuffer[Token]]()
          val tokens = currDoc.sections.flatMap(_.tokens).toSeq
          //sort tokens by y-position size -- TODO change this back to a groupBy
          tokens.foreach(token => {
            val ypos = token.attr[FormatInfo].yPos
            if (!table.contains(ypos)) table(ypos) = new ListBuffer[Token]()
            table(ypos) += token
          })
          //check
          table.foreach({ case (ypos, toks) => {
            val check = toks.map(_.attr[FormatInfo].yPos)
            assert(check.forall(i => i == check.head))
          }
          })
          // FIXME for some reason prevLines aren't sticking
          val yPositions = table.keys.toSeq
          val buf = new LineBuffer(currDoc)
          var prevLine = new Line(table(yPositions(0)), yPositions(0), prev = null)
          buf += prevLine
          var i = 1
          while (i < yPositions.length) {
            val currLine = new Line(table(yPositions(i)), yPositions(i)) //, prev=prevLine)
            buf += currLine
            prevLine = currLine
            i += 1
          }
          assert(buf.length >= 1, "buffer size = " + buf.length)
          currDoc.attr += buf
          currDoc.asSection.chainFreeze()
          docs += currDoc
          currDoc = new Document("").setName(line)
        }
      } else if (line.length > 0) {
        //        val parts = line.split("\t")//.filter(_.length > 0)
        //        assert(parts.length >= 4, s"line doesnt have at least 4 columns: ${parts.mkString(" , ")}")
        //        if (parts.length < 4)
        val parts = {
          var tmp = line.split("\t")
          if (tmp.length < 4) {
            tmp = line.split("\\s{2,}")
          }
          tmp
        }
        val Array(label, string, x, y, fontsize) = {
          if (parts.length == 5) Array(parts(0), parts(1), parts(2), parts(3), parts(4))
          else if (parts.length == 4) Array("", parts(0), parts(1), parts(2), parts(3))
          else Array("", "", "", "", "")
        }
//        println(s"line = [ $line ]\n parts = [ ${parts.mkString(",")} ] lenparts=${parts.length}")
        if (string.length > 0) {
          val token = new Token(currDoc, string)
          token.attr += new FormatInfo(token, Integer.parseInt(x), Integer.parseInt(y), Integer.parseInt(fontsize))
          if (withLabels) {
            assert(label.length > 0, "no label found")
            if (label.substring(2) != "tech" && label.substring(2) != "thesis" && label.substring(2) != "note") token.attr += new LabeledBioHeaderTag(token, label)
          }
        }
      }
    }
    docs
  }
}




