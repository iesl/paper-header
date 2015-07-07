package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.variable.CategoricalDomain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Reads in a tagged file output by Grobid, evaluates it using out evaluation
 */
object EvalGrobid extends App {
  val opts = new EvalGrobidOpts()
  opts.parse(args)

//  object cDomain extends CategoricalDomain[String]

//  class CitationLabel(labelname: String, val token: Token) extends HLabel(labelname) {
//    def domain = cDomain
//  }

  /* Load Grobid owpl output as tagged Factorie document */
  def loadGrobid(lines: Iterator[String]): IndexedSeq[Document] = {
    val docs = ArrayBuffer[Document]()
    var doc = new Document()
    var lastGold = ""
    var lastTagged = ""
    while(lines.hasNext){
      val line = lines.next()
      //    println(s"line: |$line|")
      if(line.trim == ""){
        docs += doc
        doc = new Document()
      }
      else {
        val splitLine = line.split("\t")
        val tok = new Token(doc, splitLine(0))
        val gold = splitLine(splitLine.length-2).replaceAll("<|>", "")
        val tagged = splitLine.last.replaceAll("<|>", "")
        //      val goldBI = if(gold(0) == 'I') "B" + gold.drop(1) else "I-" + gold
        //      val taggedBI = if(tagged(0) == 'I') "B" + tagged.drop(1) else "I-" + tagged
        val goldBI = if(gold(0) == 'I') "B" + gold.drop(1) else if(gold != lastGold) "B-" + gold else "I-" + gold
        val taggedBI = if(tagged(0) == 'I') "B" + tagged.drop(1) else if (tagged != lastTagged) "B-" + tagged else "I-" + tagged
//        println(s"grobid: $goldBI / $taggedBI")
//        cDomain += goldBI
        try {
          val label = new HeaderLabel(goldBI, tok)
          label.set(HeaderLabelDomain.index(taggedBI))(null)
//          val label = new CitationLabel(goldBI, tok)
//          label.set(cDomain.index(taggedBI))(null)
          tok.attr += label
        } catch {
          case e: IndexOutOfBoundsException => throw new Error(s"Label $taggedBI or $goldBI not in domain")
        }
        doc.asSection += tok
        lastGold = goldBI.drop(2)
        lastTagged = taggedBI.drop(2)
      }
    }
    docs += doc
    docs
  }

  /* Load IESL owpl output as tagged Factorie document */
  def loadIESL(lines: Iterator[String]): IndexedSeq[Document] = {
    val docs = ArrayBuffer[Document]()
    var doc = new Document()
    var lastGold = ""
    var lastTagged = ""
    while(lines.hasNext){
      val line = lines.next()
      //    println(s"line: |$line|")
      if(line.trim == ""){
        docs += doc
        doc = new Document()
      }
      else {
        val splitLine = line.split("\t")
        val tok = new Token(doc, splitLine(0))
        val gold = splitLine(splitLine.length-2)
        val tagged = splitLine.last
        val goldBI = if(gold.drop(2) != lastGold) "B" + gold.drop(1) else gold
        val taggedBI = if(tagged.drop(2) != lastTagged) "B" + tagged.drop(1) else tagged
//        val gold = splitLine(splitLine.length-2).replaceAll("<|>", "")
//        val tagged = splitLine.last.replaceAll("<|>", "")
//        //      val goldBI = if(gold(0) == 'I') "B" + gold.drop(1) else "I-" + gold
//        //      val taggedBI = if(tagged(0) == 'I') "B" + tagged.drop(1) else "I-" + tagged
//        val goldBI = if(gold(0) == 'I') "B" + gold.drop(1) else if(gold.drop(3) != lastGold) "B" + gold else "I-" + gold
//        val taggedBI = if(tagged(0) == 'I') "B" + tagged.drop(1) else if (tagged.drop(3) != lastTagged) "B" + tagged else "I-" + tagged
//        println(s"iesl: $goldBI / $taggedBI")
        try {
          val label = new HeaderLabel(goldBI, tok)
          label.set(HeaderLabelDomain.index(taggedBI))(null)
          tok.attr += label
        } catch {
          case e: IndexOutOfBoundsException => throw new Error(s"Label $taggedBI or $goldBI not in domain")
        }
        doc.asSection += tok
        lastGold = goldBI.drop(2)
        lastTagged = taggedBI.drop(2)
      }
    }
    docs += doc
    docs
  }

  val tagger = new HeaderTagger(opts.modelFile.value)

  val encoding = "iso-8859-1"
  val grobidLines = io.Source.fromFile(opts.grobidFile.value, encoding).getLines()
  val ieslLines = io.Source.fromFile(opts.ieslFile.value, encoding).getLines()

  val grobidDocs = loadGrobid(grobidLines)
  val ieslDocs = loadIESL(ieslLines)

  println(s"Grobid doc count = ${grobidDocs.length}; IESL doc count = ${ieslDocs.length}")
  println(s"Grobid tok count = ${grobidDocs.flatMap(_.tokens).length}; IESL tok count = ${ieslDocs.flatMap(_.tokens).length}")

  var i = 0
  grobidDocs.flatMap(_.tokens).zip(ieslDocs.flatMap(_.tokens)).foreach{case(gTok, iTok) => {
    i += 1
    assert(gTok.attr[HeaderLabel].target.categoryValue == iTok.attr[HeaderLabel].target.categoryValue,
      s"$i grobid: ${gTok.attr[HeaderLabel].target.categoryValue}; iesl: ${iTok.attr[HeaderLabel].target.categoryValue}")
  }}


//  println("domain: " + cDomain.categories.mkString(" "))

  println("GROBID")
//  println(new SegmentEvaluation[CitationLabel]("B-", "I-", cDomain, grobidDocs.flatMap(_.tokens).map(_.attr[CitationLabel])))
  val grobidEval = new SegmentEvaluation[HeaderLabel]("B-", "I-", HeaderLabelDomain, grobidDocs.flatMap(_.tokens).map(_.attr[HeaderLabel]))
  println(grobidEval)
  println(f"${grobidEval.f1*100}%2.2f, ${grobidEval.precision*100}%2.2f, ${grobidEval.recall*100}%2.2f")
  HeaderLabelDomain.categories.map(_.drop(2)).toSet.map{c: String => (c,grobidEval(c))}.foreach{case(c, e) => println(f"$c, ${e.f1*100}%2.2f, ${e.precision*100}%2.2f, ${e.recall*100}%2.2f")}

  println()
  println("IESL")
  val ieslEval = new SegmentEvaluation[HeaderLabel]("B-", "I-", HeaderLabelDomain, ieslDocs.flatMap(_.tokens).map(_.attr[HeaderLabel]))
  println(ieslEval)

  println(f"OVERALL, ${ieslEval.f1*100}%2.2f, ${ieslEval.precision*100}%2.2f, ${ieslEval.recall*100}%2.2f")
  HeaderLabelDomain.categories.map(_.drop(2)).toSet.map{c: String => (c, ieslEval(c))}.foreach{case(c,e) => println(f"$c, ${e.f1*100}%2.2f, ${e.precision*100}%2.2f, ${e.recall*100}%2.2f")}

}

class EvalGrobidOpts extends cc.factorie.util.DefaultCmdOptions {
  /* data */
  val grobidFile = new CmdOption("grobid-file", "", "STRING", "Filename(s) from which to read data")
  val ieslFile = new CmdOption("iesl-file", "", "STRING", "Filename(s) from which to read data")
  val modelFile = new CmdOption("model", "", "STRING", "Location of header tagger file")
}