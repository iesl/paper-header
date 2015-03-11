package edu.umass.cs.iesl.paperheader.tagger

import cc.factorie.app.nlp._
import org.scalatest._
import edu.umass.cs.iesl.paperheader.load._
import scala.collection.mutable
import scala.io.Source

/**
* @author Kate Silverstein
*         created on 2/4/15
*/
class TestHeaderTagger extends FlatSpec {
//  "LoadCitations" should "load stuff" in {
//    val fname = System.getenv("PH_ROOT") + "/umass-citation/dev.docs"
//    val docs = LoadCitation.loadFromFile(fname)
//    println(docs.length)
//  }

//  val phRoot = System.getenv("PH_ROOT")
//  val dataPath = phRoot + "/data/fullpaper-headers.tsv"
//  "LoadTSV" should "load data" in {
//    println(s"PH_ROOT=$phRoot")
//    val (trainDocs, _, _) = LoadTSV.loadDataSets(dataPath, BILOU=true)
//    trainDocs.take(5).foreach(doc => {
//      doc.sections.flatMap(_.tokens).foreach(token => {
//        assert(token.attr != null)
//        assert(token.attr.contains(classOf[LabeledBioHeaderTag]), s"token ${token.string} with no LabeledBioHeaderTag")
//        assert(token.attr.contains(classOf[LabeledBilouHeaderTag]), s"token ${token.string} with no LabeledBilouHeaderTag")
//        assert(token.attr[LabeledBilouHeaderTag] != null, s"token ${token.string} with no LabeledBilouHeaderTag")
//        val sentLen = token.sentence.length
//        val currPos = token.positionInSentence
//        val outString = s"${token.string} has no "
//        if (currPos > 1) {
//          assert(token.hasPrev, outString + "prev (hasPrev)")
//          assert(token.sentenceHasPrev, outString + "prev (sentenceHasPrev)")
//          assert(token.prev != null, outString + "prev (prev is null)")
//        }
//        if (currPos < sentLen - 2) {
//          assert(token.hasNext, outString + "next (hasNext)")
//          assert(token.sentenceHasNext, outString + "next (sentenceHasNext)")
//          assert(token.next != null, outString + "next (next is null)")
//        }
//      })
//    })
//  }


}
