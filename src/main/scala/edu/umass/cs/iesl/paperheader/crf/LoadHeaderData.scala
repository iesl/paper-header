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

class LoadTSV(val withLabels: Boolean = true, separator: String = "#") extends Load {
  override def fromFile(file:java.io.File): Seq[nlp.Document] = {
    val docs = fromSource(Source.fromFile(file))
    println(s"Loaded ${docs.length} docs from ${file.getName}")
    docs
  }

  def fromSource(source:Source): Seq[nlp.Document] = {
    println("LOADTSV: withLabels = " + withLabels)
    val docs = new mutable.ListBuffer[nlp.Document]()
    val l = source.getLines().toSeq
    val firstLineLen = l(0).length
    val lines = if (firstLineLen == 0) l.drop(1) else l
    val docName = lines(0)

    assert(lines(0).startsWith(separator), lines(0))
    var currDoc = new nlp.Document("").setName(lines(0))

    for (line <- lines) {
      if (line.startsWith(separator)) {
        //found a new document
        if (currDoc.tokenCount > 0) {
          val table = new mutable.HashMap[Int, mutable.ListBuffer[nlp.Token]]()
          val tokens = currDoc.sections.flatMap(_.tokens).toSeq
          //sort tokens by y-position size -- TODO change this back to a groupBy
          tokens.foreach(token => {
            val ypos = token.attr[FormatInfo].yPos
            if (!table.contains(ypos)) table(ypos) = new mutable.ListBuffer[nlp.Token]()
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
          currDoc = new nlp.Document("").setName(line)
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
          val token = new nlp.Token(currDoc, string)
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


/**
 * Load data for the HeaderTagger to process.
 *
 * Expects one big file with at least 4 tab-separated columns (5 columns if withLabels = true), which are:
 *
 * (BIO-label) string xpos ypos fontsize
 *
 */
object LoadTSV {
  /**
   *
   * @param filename
   * @param withLabels - if true, then tokens will be labeled with a gold LabeledHeaderTag
   * @return
   */
  def apply(filename:String, withLabels:Boolean = false) = { new LoadTSV(withLabels=withLabels).fromFilename(filename)}
}


object LoadTester {
  def main(args:Array[String]): Unit = {
        val path = "/iesl/canvas/ksilvers/paperheader/data/fullpaper-headers.tsv"
//    val path = "/Users/kate/research/paperheader/fullpaper-headers.tsv"

    val docs = LoadTSV(path, false)
//    val docs = LoadTSV(path, true)
//    //    assert(docs.length >= 2)
    println(s"got ${docs.length} docs")
    docs.take(10).foreach(doc => {
      val toks = doc.sections.flatMap(_.tokens).toSeq
      val string = toks.map(_.string).mkString(" , ")
      println(string)
      println("")
    })
  }
}

