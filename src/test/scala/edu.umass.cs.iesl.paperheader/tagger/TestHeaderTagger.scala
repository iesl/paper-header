package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp._
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
  
//  "LoadTSV" should "load citation data" in {
//    val path = phRoot + "/data/cites.tsv"
//    val docs = LoadTSV(path)
//    print(docs.length)
//    assert(docs.length > 0)
//
//  }

  //FIXME LoadTSV should load 445 docs
  "LoadTSV" should "load 444 docs with 99475 tokens and none should be empty; first four tokens should be Yale,University,Department,of" ignore {
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
  
  it should "load all docs with FormatInfo attr" ignore {
    val vf = (t:Token) => t.attr[FormatInfo]
    val docs = LoadTSV(dataPath, withLabels=true)
    assert(docs.length == 445)
    docs.foreach(doc => assert(doc.tokenCount > 0, "found empty doc"))
    docs.foreach(doc => {
      val toks = doc.sections.flatMap(_.tokens)
      toks.foreach(t => {
        assert(t.attr[FormatInfo] != null, s"found token ${t.string} without FormatInfo")
//        assert(vf(t) != null)
        val f = t.attr[FormatInfo]
        assert(f.fontsize >= 0, "invalid font size")
        assert(f.xpos >= 0, "invalid xpos")
      })
      
    })
    
  }
}
