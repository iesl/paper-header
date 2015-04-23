package edu.umass.cs.iesl.paperheader.load

/**
 * @author Kate Silverstein 
 *         created on 4/22/15
 */

import java.time.Year

import scala.util.Random._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import cc.factorie.app.nlp
import cc.factorie.app.nlp.segment.DeterministicLexerTokenizer
import edu.umass.cs.iesl.paperheader.tagger._
import scala.xml._
/*
TODO same tagset as Grobid
TODO train and validate on same data Grobid uses to train/validate
TODO segeval on Grobid output for test?
 */
/*
    val testDocs = opts.dataSet.value match {
      case "grobid" => LoadGrobid.loadDataFromDir(opts.testDir.value)
      case "fp" => LoadTSV.loadTSV(opts.test.value, BILOU=true)
      case "all" => shuffle(LoadGrobid.loadDataFromDir(opts.testDir.value) ++ LoadTSV.loadTSV(opts.test.value, BILOU=true))
      case _ => throw new Exception("TODO")
    }
 */
object GrobidProcess {
  import sys.process._
  def writeData(dir: String, outDir: String): Unit = {
    val files = new File(dir).listFiles
    for (file <- files) {
      val doc = LoadGrobid.loadGrobidTSV(file.getPath)
      val tokenGrps = doc.tokens.groupBy { t =>
        val tag = t.attr[LabeledBilouHeaderTag].categoryValue
        if (tag == "O") "O"
        else tag.substring(2)
      }
//      val str = doc.tokens.map(_.string).mkString(" ")
      val d = new nlp.Document("")
      var str = ""
      tokenGrps.foreach { case (tag, toks) =>
          str += toks.map(_.string).mkString(" ") + "\n\n"
      }
      val pw = new PrintWriter(new File(s"$outDir/${file.getName}"))
      pw.write(str)
      pw.close()
    }
  }
  def main(args: Array[String]): Unit = {
    val tsvDir = args(0)
    val bareOutputDir = args(1)
    writeData(tsvDir, bareOutputDir)
    val outputDir = args(2)
    val files = new File(bareOutputDir).listFiles
    for (f <- files) {
      val output = s"$outputDir/${f.getName}"
      val cmd = s"pandoc ${f.getAbsolutePath} -o $output.pdf"
      Process(cmd).run()
    }
  }

}

/*
x <title>		96.9		90.91		76.92		83.33
x <author>		94.57		85.71		70.59		77.42
x <affiliation>	92.25		73.33		64.71		68.75
x <address>		92.64		74.07		62.5		67.8
<pubnum>		98.45		0		0		0
x <date>		97.29		0		0		0
<note>		93.41		13.33		33.33		19.05
x <email>		96.51		80.95		77.27		79.07
x <abstract>		95.35		81.25		59.09		68.42
<intro>		95.74		10		33.33		15.38
x <grant>		98.84		0		0		0
x<submission>	98.45		0		0		0
x<web>		98.84		0		0		0
x <phone>		98.84		0		0		0
x<keyword>		98.84		33.33		50		40
x <reference>		99.61		0		0		0

 */

class Extractor(root: NodeSeq) {
  def apply(pattern: String): Seq[String] = {
    val parts = pattern.split(">")
    parts.length match {
      case 1 => (for (b <- root \\ parts(0)) yield b.text).filter(_.length > 0)
      case 2 => parts(1).split("=").length match {
        case 1 => (for (b <- root \\ parts(0)) yield (b \\ parts(1)).text).filter(_.length > 0)
        case 2 =>
          val parent = parts(0); val parentType = parts(1).split("=")(1)
          (root \\ parts(0)).map(elem => (elem \ "@type", elem.text)).filter(_._1.text == parentType).map(_._2)
      }
    }
  }
}

object Extractor {
  def apply(root: NodeSeq, pattern: String): Seq[String] = new Extractor(root).apply(pattern)
}


/** Read in Grobid data (TEI/XML) and output labeled FACTORIE docs **/
class GrobidMung {
  def getExtractor(n: NodeSeq, p: String) = Extractor(n, p)
  def extractor(p: String) = { root: NodeSeq => Extractor(root, p) }
  val elems = Map(
    "abstract" -> { root: NodeSeq => Extractor(root, "div>type=abstract") },
    "author" -> { root: NodeSeq => Extractor(root, "byline>docAuthor") },
    "affiliation" -> { root: NodeSeq => Extractor(root, "byline>affiliation") },
    "address" -> { root: NodeSeq => Extractor(root, "address") },
    "date" -> { root: NodeSeq => Extractor(root, "date") },
    "email" -> { root: NodeSeq => Extractor(root, "email") },
    "grant" -> { root: NodeSeq => Extractor(root, "note>type=grant") },
    "keyword" -> { root: NodeSeq => Extractor(root, "keywords") },
    "phone" -> { root: NodeSeq => Extractor(root, "note>type=phone") },
    "reference" -> { root: NodeSeq => Extractor(root, "note>type=reference") },
    "submission" -> { root: NodeSeq => Extractor(root, "note>type=submission") },
    "title" -> { root: NodeSeq => Extractor(root, "docTitle>titlePart") },
    "web" -> { root: NodeSeq => Extractor(root, "ptr>type=web")}
  )


  def processXml(file: File): nlp.Document = {
    val doc = new nlp.Document("")
    val spanBuff = new GrobidTagSpanBuffer
    val xml = scala.xml.XML.loadFile(file)
    val headerRoot = xml \ "text" \ "front"
    val labeledSpans = elems.map { case (tag, extract) => (tag, extract(headerRoot)) }
    scala.util.Random.shuffle(labeledSpans).foreach { case (tag, stringSeq) =>
      val d = new nlp.Document("")
      stringSeq.foreach(s => d.appendString(s))
      cc.factorie.app.nlp.segment.DeterministicTokenizer.process(d)
      val span = new GrobidTagSpan(d.asSection, 0, d.tokens.size, tag)
      span.mkBilouTokens()
      span.tokens.foreach(t => {
        val t2 = new nlp.Token(doc, t.string)
        t2.attr += new LabeledBilouGrobidTag(t2, t.attr[LabeledBilouGrobidTag].categoryValue)
      })
    }
//    elems
//      .map { case (tag, extract) => (tag, extract(headerRoot)) }
//      .foreach { case (tag, stringSeq) =>
//      val d = new nlp.Document("")
//      stringSeq.foreach(s => d.appendString(s))
//      cc.factorie.app.nlp.segment.DeterministicTokenizer.process(d)
//      val span = new GrobidTagSpan(d.asSection, 0, d.tokens.size, tag)
//      span.mkBilouTokens()
//      span.tokens.foreach(t => {
//        val t2 = new nlp.Token(doc, t.string)
//        t2.attr += new LabeledBilouGrobidTag(t2, t.attr[LabeledBilouGrobidTag].categoryValue)
//      })
//    }
    //    spanBuff ++= spans
    //    doc.attr += spanBuff
    doc
  }

  def mungFiles(grobidRoot: String): Seq[nlp.Document] = {
    val files = new File(grobidRoot).listFiles
    println(s"loading ${files.length} files from $grobidRoot")
    files.map(f => processXml(f))
  }


}

//object GrobidMung {
//  def main(args: Array[String]): Unit = {
//    val groot = args(0)
//    val mung = new GrobidMung(groot)
//    mung.mungFiles()
//  }
//}

object LoadGrobid {
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
  def loadDataSetsFromDir(dir: String): (Seq[nlp.Document], Seq[nlp.Document], Seq[nlp.Document]) = {
    val fileList = new java.io.File(dir).listFiles()
    val ad = new ArrayBuffer[nlp.Document]()
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

  def loadDataFromDir(dir: String): Seq[nlp.Document] = {
    println(s"LoadGrobid: loading data from $dir")
    val fileList = new java.io.File(dir).listFiles()
    val ad = new ArrayBuffer[nlp.Document]()
    for (file <- fileList) {
      ad += loadGrobidTSV(file.getAbsolutePath)
    }
    shuffle(ad).toSeq
  }

  def loadGrobidTSV(filename:String): nlp.Document = {
    val doc = new nlp.Document("")
    doc.annotators(classOf[nlp.Sentence]) = nlp.UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    var sentence = new nlp.Sentence(doc)
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
        if (tagMap.contains(baseLabel)) {
          if (currLabel != baseLabel) {
            sentence = new nlp.Sentence(doc)
            currLabel = baseLabel
          }
          if (string.length > 0) {
            val token = new nlp.Token(sentence, string)
            token.attr += new LabeledBilouHeaderTag(token, label)
          }
        }
      }
    }
    doc
  }
}