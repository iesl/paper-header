package edu.umass.cs.iesl.paperheader.load

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.paperheader.tagger.{FormatInfo, LabeledBioHeaderTag, LabeledBilouHeaderTag, Line, LineBuffer}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random._

/**
 * Created by kate on 9/25/14.
 */

object LoadGrobid {
  def loadDataSetsFromDir(dir: String): (Seq[Document], Seq[Document], Seq[Document]) = {
    val fileList = new java.io.File(dir).listFiles()
    val ad = new mutable.ArrayBuffer[Document]()
    for (file <- fileList) {
      ad += loadGrobidTSV(file.getAbsolutePath)
    }
    val allDocs = shuffle(ad)
    val trainPortion = (allDocs.length * 0.7).floor.toInt
    val trainDocs = allDocs.take(trainPortion)
    val restDocs = allDocs.drop(trainPortion)
    val devPortion = (restDocs.length * 0.8).floor.toInt
    val devDocs = restDocs.take(devPortion)
    val testDocs = restDocs.drop(devPortion)
//    println(s"LoadGrobid: loaded ${allDocs.length} docs total: train=${trainDocs.length}, dev=${devDocs.length}, test=${testDocs.length}")
    (trainDocs.toSeq, devDocs.toSeq, testDocs.toSeq)
  }

  def loadDataFromDir(dir: String): Seq[Document] = {
    println(s"LoadGrobid: loading data from $dir")
    val fileList = new java.io.File(dir).listFiles()
    val ad = new mutable.ArrayBuffer[Document]()
    for (file <- fileList) {
      ad += loadGrobidTSV(file.getAbsolutePath)
    }
    shuffle(ad).toSeq
  }

  def loadGrobidTSV(filename:String): Document = {
    val doc = new Document("")
    doc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    var sentence = new Sentence(doc)
    var currLabel = ""
    val lines = Source.fromFile(filename).getLines()
    for (line <- lines) {
      val parts = line split "\t"
      //      assert(parts.length == 2, "line len not 2: "+line+" in file: " + filename)
      if (parts.length != 2) {
        println("line len not 2: " + line + " in file: " + filename)
      } else {
        val label = parts(0)
        val string = parts(1)
        val baseLabel = label.split("-")(1)
        if (currLabel != baseLabel) {
          sentence = new Sentence(doc)
          currLabel = baseLabel
        }
        if (string.length > 0) {
          val token = new Token(sentence, string)
          token.attr += new LabeledBilouHeaderTag(token, label)
        }
      }
    }
    doc
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
  val tagSet = Seq(
    "title",
    "author",
    "affiliation",
    "address",
    "email",
    "date",
    "abstract",
    "phone",
    "keyword",
    "web"
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
  def loadTSV(filename: String, BILOU:Boolean=false): Seq[Document] = {
    val docs = new mutable.ListBuffer[Document]()
    //    val lines = Source.fromFile(filename).getLines().toSeq.drop(1)
    //    assert(lines(0).startsWith("#"), s"first line should start with '#', instead starts with '${lines(0)(0)}'!")
    val lines1 = Source.fromFile(filename).getLines().toSeq
    val firstLine = lines1(0)
    val lines: Seq[String] = {
      if (firstLine.length == 0) lines1.drop(2)
      else lines1
    }
//    print(s"first line: ${lines(0)}")
    var doc = new Document("")
    var sentence = new Sentence(doc)
    var currLabel = ""
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
            val baseTag = label.substring(2)
            if (baseTag != currLabel) {
              sentence = new Sentence(doc)
              currLabel = baseTag
            }
            val token = new Token(sentence, string)
            token.attr += new LabeledBioHeaderTag(token, label)
          }
        }
      }
    })
    // take care of end case
    if (doc.tokenCount > 0) docs += doc
    if (BILOU) convertToBILOU(docs)
    shuffle(docs)
  }

  def convertToBILOU(documents : mutable.ListBuffer[Document]) {
    for (doc <- documents) {
      doc.sections.flatMap(_.tokens).foreach(token => {
        //println("=======")
        val ner = token.attr[LabeledBioHeaderTag]
        var prev : Token = null
        var next : Token = null
        //println(token + " -> " + ner.categoryValue);
        //          if(token.sentenceHasPrev) prev = token.sentencePrev
        //          if(token.sentenceHasNext) next = token.sentenceNext
        //          token.sentenceNext
        if (token.hasPrev) prev = token.prev
        if (token.hasNext) next = token.next
        /*
        if(prev != null)
          println(prev + " -> " + prev.nerLabel.categoryValue);
        if(next != null)
          println(next + " -> " + next.nerLabel.categoryValue); */
        val newLabel : String = IOBtoBILOU(prev, token, next)
        /*if(token.string == "Peter")
          println(newLabel)
        if(token.prev != null && token.prev.string == "Peter") {
          println("Peter Prev")
          println(token.string)
          println(newLabel)
        }*/
//        token.attr.remove[IobConllNerLabel]
//        token.attr.remove[LabeledBioHeaderTag]
        token.attr += new LabeledBilouHeaderTag(token, newLabel)
      })
    }
  }

  def IOBtoBILOU(prev : Token, token : Token,  next : Token) : String = {
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

  private def splitLabel(token : Token) : Array[String] = {
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
  def apply(filename:String, withLabels:Boolean = false, withFormatting: Boolean = false): Seq[Document] = {
    if (withFormatting) loadTSVWithFormatInfo(filename)
    else loadTSV(filename)
  }

  def loadDataSets(filename:String, BILOU:Boolean=false): (Seq[Document], Seq[Document], Seq[Document]) = {
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

