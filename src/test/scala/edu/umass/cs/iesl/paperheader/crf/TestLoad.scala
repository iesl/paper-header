package edu.umass.cs.iesl.paperheader.crf

import org.scalatest.FlatSpec

class TestLoad extends FlatSpec {
  val tsvTestFilename = getClass.getResource("/load-tsv-test-input").getPath
  "LoadTSV" should "load 1 document with 16 tokens" in {
    val docs = LoadTSV(tsvTestFilename)
    assert(docs.length == 1, "failed to load docs")
    assert(docs.head.tokenCount == 16, "wrong token count")
    val tokens = docs.head.sections.flatMap(_.tokens)
    tokens.foreach(token => assert(token.attr.contains(classOf[BioHeaderTag]), "token with no BioHeaderTag"))
  }
}