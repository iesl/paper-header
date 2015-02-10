package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp
import org.scalatest._

import scala.collection.mutable
import scala.io.Source

/**
 * @author Kate Silverstein 
 *         created on 2/4/15
 */
class TestHeaderTagger extends FlatSpec {
  val phRoot = System.getenv("PH_ROOT")
  val dataPath = phRoot + "/data/fullpaper-headers-modified.tsv"
  val tagSet = LoadTSV.tagSet

  //FIXME LoadTSV should load 445 docs
  "LoadTSV" should "load 444 docs with 99475 tokens and none should be empty; first four tokens should be Yale,University,Department,of" in {
    val docs = LoadTSV(dataPath, withLabels=true)
    assert(docs.length == 445)
    docs.foreach(doc => assert(doc.tokenCount > 0, "found empty doc"))
    val tokens = docs(0).sections.flatMap(_.tokens).toIndexedSeq
    val tests = Seq("Yale", "University", "Department", "of", "Computer")
    (0 until 5).foreach(i => {
      val tok = tokens(i).string
      assert(tok == tests(i), s"$i $tok != ${tests(i)}")
    })
  }
}
