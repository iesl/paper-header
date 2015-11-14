package edu.umass.cs.iesl.paperheader

import java.io._

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import edu.umass.cs.iesl.paperheader.load.{LoadGrobid, LoadIESL}
import edu.umass.cs.iesl.paperheader.model._


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
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new DefaultHeaderTagger(lexicon)
    tagger.deserialize(new FileInputStream(new File(opts.modelFile.value)))
    docs.foreach(tagger.process)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

  def runGrobid(opts: HeaderTaggerOpts): Unit = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    val tagger = new GrobidHeaderTagger
    tagger.deserialize(new FileInputStream(new File(opts.modelFile.value)))
    docs.foreach(tagger.process)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

  def runCombined(opts: HeaderTaggerOpts): Unit = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val tagger = new CombinedHeaderTagger(lexicon)
    tagger.deserialize(new FileInputStream(new File(opts.modelFile.value)))
    docs.foreach(tagger.process)
    val labels = docs.flatMap(doc => doc.tokens.map(_.attr[GoldHeaderTag])).toIndexedSeq
    println(new SegmentEvaluation[GoldHeaderTag]("(B|U)-", "(I|L)-", HeaderDomain, labels))
  }

}
