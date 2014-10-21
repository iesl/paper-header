package edu.umass.cs.iesl.paperheader

import cc.factorie.variable._
import cc.factorie.app.nlp
import cc.factorie.app.nlp.load._
import scala.io.Source
import scala.xml._

import scala.util.matching.Regex
import scala.collection.mutable.{HashMap, ListBuffer}

/**
 * Created by kate on 9/25/14.
 */

//TODO add parameterization for grouping TextBlocks together? e.g. if (fontSize) groupBy fontSize
object LoadTSV extends Load {

  override def fromFile(file:java.io.File): Seq[nlp.Document] = {
    val docs = fromSource(Source.fromFile(file))
    println(s"Loaded ${docs.length} docs from ${file.getName}")
    docs
  }

  def fromSource(source:Source): Seq[nlp.Document] = {
    val docs = new ListBuffer[nlp.Document]()
    val lines = source.getLines().drop(1).toSeq //first line should be blank (see fullpaper-headers.tsv)
    assert(lines(0).startsWith("#"), lines(0))

    var currDoc = new nlp.Document("").setName(lines(0))
//    var startOffset = 0; var endOffset = 0;
    for (line <- lines.drop(1)) {
      if (line.startsWith("#")) {
        //found a new document

        val table = new HashMap[Int, ListBuffer[nlp.Token]]()
        val tokens = currDoc.sections.flatMap(_.tokens).toSeq
        //sort tokens by y-position size -- TODO change this back to a groupBy
        tokens.foreach(token => {
          val ypos = token.attr[FormatInfo].yPos
          if (!table.contains(ypos)) table(ypos) = new ListBuffer[nlp.Token]()
          table(ypos) += token
        })
        //check
        table.foreach({case (ypos, toks) => {
          val check = toks.map(_.attr[FormatInfo].yPos)
          assert(check.forall(i => i == check.head))
        }})

        // FIXME for some reason prevLines aren't sticking
        val yPositions = table.keys.toSeq
        val buf = new LineBuffer(currDoc)
        var prevLine = new Line(table(yPositions(0)), yPositions(0), prev=null)
        buf += prevLine
        var i = 1
        while (i < yPositions.length) {
          val currLine = new Line(table(yPositions(i)), yPositions(i), prev=prevLine)
          buf += currLine
          prevLine = currLine
          i += 1
        }
        assert(buf.length >= 1)
        currDoc.attr += buf

        currDoc.asSection.chainFreeze()
        docs += currDoc
        currDoc = new nlp.Document("").setName(line)
//        startOffset = endOffset+1
//        endOffset += 1

      } else if (line.length > 0) {
        val parts = line.split("\t")
        assert(parts.length == 5)
        val Array(label, string, x, y, fontsize) = parts
        if (label.substring(2) != "tech" && label.substring(2) != "thesis" && label.substring(2) != "note") {
          val token = new nlp.Token(currDoc, string)
          token.attr += new LabeledBioHeaderTag(token, label)
          token.attr += new FormatInfo(token, Integer.parseInt(x), Integer.parseInt(y), Integer.parseInt(fontsize))
//          endOffset += string.length
        }
      }
    }

    docs
  }
}


object LoadTester {
  def main(args:Array[String]): Unit = {
    val path = "/iesl/canvas/ksilvers/paperheader/data/fullpaper-headers.tsv"
    val docs = LoadTSV.fromFilename(path)
//    assert(docs.length >= 2)
    println(s"got ${docs.length} docs")
    docs.take(10).foreach(doc => {
      val toks = doc.sections.flatMap(_.tokens).toSeq
      val string = toks.map(_.string).mkString(" , ")
      println(string)
      println("")
    })

//    docs.take(2).foreach(doc => {
//      val b = doc.attr[TextBlockBuffer]
//      println(s"${doc.name} with ${b.length} textblocks")
//      b.blocks.foreach(block => { val fs = block.getYVals; assert(fs.forall(i => i == fs.head))})
//      b.blocks.foreach(block => println(block.getYVals.mkString(" ")))
//      println("")
//    })


  }
}

/** Incomplete loader for data/tagged_headers.txt **/
object LoadHeaderSGML extends Load {
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
          // doc.appendString(" ")
          val newline = new nlp.Token(doc, "\n")
          newline.attr += new LabeledBilouHeaderTag(newline, "O")
        })
      })
    } catch {
      case _ : Throwable => //println("FIXME: Unknown exception: should be handling SAXParseException because of possibly malformed XML")
    }
    doc.asSection.chainFreeze()
    doc
  }

  def fromSource(source:Source): Seq[nlp.Document] = {
    val docs = new ListBuffer[nlp.Document]()
    val lines = source.getLines().toSeq
    val lineSeq = new ListBuffer[String]()
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
    docs.filter(_.tokenCount > 0)
  }

  override def fromFile(file:java.io.File): Seq[nlp.Document] = {
    val docs = fromSource(Source.fromFile(file))
    println(s"Loaded ${docs.length} docs from ${file.getName}")
    docs
  }
}
