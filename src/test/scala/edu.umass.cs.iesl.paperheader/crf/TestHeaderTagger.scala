package edu.umass.cs.iesl.paperheader.crf

import org.scalatest._

/**
 * @author Kate Silverstein 
 *         created on 2/4/15
 */
class TestHeaderTagger extends FlatSpec {
  val phRoot = System.getenv("PH_ROOT")
  val dataPath = phRoot + "/data/fullpaper-headers.tsv"

  //FIXME LoadTSV should load 445 docs
  "LoadTSV" should "load 444 docs with 99475 tokens" in {
    val docs = LoadTSV(dataPath, withLabels=true)
    assert(docs.length == 444)
    val numToks = docs.map(_.tokenCount).sum
    assert(numToks == 99475)
  }

  "Loader.loadTSVSimple" should "load 445 docs with 99655 tokens" in {
    val docs = Loader.loadTSVSimple(dataPath)
    println(s"loaded ${docs.length}")
    assert(docs.length == 445, s"loaded ${docs.length} (should be 444)")
    val numToks = docs.map(_.tokenCount).sum
    println(s"loaded $numToks tokens")
    assert(numToks == 99655, s"loaded $numToks (should be 99475)")
  }

  "Loader.loadTSV" should "load 445 docs, all should have LineBuffer" in {
    val docs = Loader.loadTSV(dataPath)
    println(s"loaded ${docs.length}")
    docs.foreach(doc => {
      assert(doc.attr[LineBuffer] != null, "found doc with null LineBuffer")
      assert(doc.attr[LineBuffer].length > 0, "found doc with empty LineBuffer")
    })
    //print a sample
    docs.take(3).foreach(doc => {
      if (doc.attr[LineBuffer].length > 3) doc.attr[LineBuffer].blocks.take(3).foreach(b => println(b.toString))
      println()
    })
  }

  "tokens" should "all have attr[LabeledBioHeaderTag] after loadTSVSimple" in {
    val docs = Loader.loadTSVSimple(dataPath)
    docs.foreach(doc => {
      val tokens = doc.sections.flatMap(_.tokens)
      assert(tokens.length > 0, "doc with no tokens found")
      tokens.foreach(t => assert(t.attr[LabeledBioHeaderTag] != null, s"token ${t.string} with no LabeledBioHeaderTag"))
    })
//    //print a sample
//    docs.take(3).foreach(doc => {
//      val tokens = doc.sections.flatMap(_.tokens)
//      tokens.foreach(t => {
//        println(s"${t.string} ${t.attr[LabeledBioHeaderTag].categoryValue}")
//      })
//    })
  }

  "Eval" should "execute w.out error" in {
    val docs = Loader.loadTSV(dataPath)
    val labels = new scala.collection.mutable.ListBuffer[LabeledBioHeaderTag]()
    docs.foreach(doc => {
      doc.attr[LineBuffer].blocks.foreach(line => {
        line.tokens.foreach(tok => {
          tok.attr += new BioHeaderTag(tok, "O")
          labels += tok.attr[LabeledBioHeaderTag]
        })
      })
    })
    Eval(BioHeaderTagDomain, labels.toIndexedSeq)
  }

}
