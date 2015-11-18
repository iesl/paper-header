package edu.umass.cs.iesl.paperheader

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import edu.umass.cs.iesl.paperheader.load.{LoadGrobid, LoadIESL}
import edu.umass.cs.iesl.paperheader.model._

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by kate on 11/14/15.
 */
object App {

  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.taggerType.value match {
      case TAGGER_TYPE_DEFAULT => runDefault(opts)
      case TAGGER_TYPE_GROBID => runGrobid(opts)
      case TAGGER_TYPE_COMBINED => runCombined(opts)
      case _ => throw new Exception(s"bad tagger-type option: ${opts.taggerType.value}")
    }
  }

  def runDefault(opts: HeaderTaggerOpts): Unit = {
    val docs = LoadIESL.fromFilename(opts.testFile.value)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new DefaultHeaderTagger(None, lexicon, opts.modelFile.value)
    docs.foreach(tagger.process)
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

  def runGrobid(opts: HeaderTaggerOpts): Unit = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    datasetInfo(docs)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    val tagger = new GrobidHeaderTagger(None, opts.modelFile.value)
    docs.foreach(tagger.process)
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

  def runCombined(opts: HeaderTaggerOpts): Unit = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new CombinedHeaderTagger(None, lexicon, opts.modelFile.value)
    docs.foreach(tagger.process)
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

  def check(docs: Seq[Document]): Unit = {
    docs.take(3).foreach { doc =>
      doc.tokens.foreach { token =>
        println(s"${token.string}\t${token.attr[GoldHeaderTag].target.categoryValue}")
      }
      println("")
    }
  }

  def datasetInfo(docs: Seq[Document]): Unit = {
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq

    val table = new mutable.HashMap[String, Int]()
    val n = labels.length
    var i = 0
    while (i < n) {
      val curr = labels(i)
      val tag = {
        val bilou = curr.target.categoryValue
        bilou.split("-").last
      }
      if (table.contains(tag)) table(tag) += 1
      else table(tag) = 1
      i += 1
    }
    println(s"ndocs: ${docs.length}, ntokens: ${labels.length}")
    table.foreach { case (cat, count) => println(s"$cat\t$count")}
  }

}
