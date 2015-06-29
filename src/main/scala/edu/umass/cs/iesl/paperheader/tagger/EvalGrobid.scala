package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Token, Document}

import scala.collection.mutable.ArrayBuffer

/**
 * Reads in a tagged file output by Grobid, evaluates it using out evaluation
 */
object EvalGrobid extends App {
  val opts = new EvalGrobidOpts()
  opts.parse(args)

  val tagger = new HeaderTagger(opts.modelFile.value)

  val encoding = "iso-8859-1"
  val dataLines = io.Source.fromFile(opts.dataFile.value, encoding).getLines()

  /* Load Grobid output as tagged Factorie document */
  val docs = ArrayBuffer[Document]()
  var doc = new Document()
  while(dataLines.hasNext){
    val line = dataLines.next()
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
      val goldBI = if(gold(0) == 'I') "B" + gold.drop(1) else "I-" + gold
      val taggedBI = if(tagged(0) == 'I') "B" + tagged.drop(1) else "I-" + tagged
      println(s"$goldBI / $taggedBI")
      try {
        val label = new HeaderLabel(goldBI, tok)
        label.set(HeaderLabelDomain.index(taggedBI))(null)
        tok.attr += label
      } catch {
        case e: IndexOutOfBoundsException => throw new Error(s"Label $taggedBI or $goldBI not in domain")
      }
      doc.asSection += tok
    }
  }
  docs += doc


  println(new SegmentEvaluation[HeaderLabel]("(B|U)-", "(I|L)-", HeaderLabelDomain, docs.flatMap(_.tokens).map(_.attr[HeaderLabel])))

}

class EvalGrobidOpts extends cc.factorie.util.DefaultCmdOptions {
  /* data */
  val dataFile = new CmdOption("data-file", "", "STRING", "Filename(s) from which to read data")
  val modelFile = new CmdOption("model", "", "STRING", "Location of header tagger file")
}