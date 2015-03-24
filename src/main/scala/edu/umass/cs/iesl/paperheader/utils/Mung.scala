package edu.umass.cs.iesl.paperheader.utils

/**
 * @author Kate Silverstein 
 *         created on 3/23/15
 */

import edu.umass.cs.iesl.paperheader.crf._
import cc.factorie.app.nlp.Document
import scala.util.Random.shuffle
import java.io._

object Mung {
  def main(args: Array[String]): Unit = partitionHeaderData(args(0))

  /** Split and write out fullpaper-headers.tsv to fullpaper-headers.train, fullpaper-headers.dev, fullpaper-headers.test
    * where train is 70%, dev is 10%, and test is 20%
    * @param filename
    */
  def partitionHeaderData(filename: String): Unit = {
    val docs = shuffle(LoadTSV(filename))
    println(s"loaded ${docs.length} docs total")
    docs.foreach(doc => {
      val toks = doc.sections.flatMap(_.tokens)
      toks.foreach(t => {
        if (!t.attr.contains(classOf[LabeledBioHeaderTag])) {
          println("token with no HeaderTag:")
          println(s"${t.string}")
          println(s"${t.attr.toString()}")
        }
      })
//      assert(toks.forall(t => t.attr.contains(classOf[LabeledBioHeaderTag])), "some token has no label")
    })
    val trainPortion = (docs.length * 0.7).floor.toInt
    val trainDocs = docs.take(trainPortion)
    val restDocs = docs.drop(trainPortion)
    val devPortion = (restDocs.length * 0.33).floor.toInt
    val devDocs = restDocs.take(devPortion)
    val testDocs = restDocs.drop(devPortion)
    println(s"${trainDocs.length} train, ${devDocs.length} dev, ${testDocs.length} test")
    writeDocs(trainDocs, "fullpaper-headers.train")
    writeDocs(devDocs, "fullpaper-headers.dev")
    writeDocs(testDocs, "fullpaper-headers.test")
  }
  def writeDocs(docs: Seq[Document], filename: String): Unit = {
    val pw = new PrintWriter(new File(filename))
    var ct = 0
    for (doc <- docs) {
      writeDoc(pw, doc, s"fullpaper-headers-$ct")
      ct += 1
    }
    pw.close()
  }
  def writeDoc(pw: PrintWriter, doc: Document, docID: String): Unit = {
    pw.write(s"#\t$docID\n")
    doc.sections.flatMap(_.tokens).foreach(token => {
      assert(token.attr.contains(classOf[LabeledBioHeaderTag]))
      assert(token.attr[LabeledBioHeaderTag] != null)
      val line = s"${token.attr[LabeledBioHeaderTag].categoryValue}\t${token.string}\n"
      pw.write(line)
    })
  }
}
