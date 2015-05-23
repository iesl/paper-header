//package edu.umass.cs.iesl.paperheader.load
//
//import java.io._
//
//import cc.factorie.app.nlp.Document
//import edu.umass.cs.iesl.paperheader.tagger.LabeledBioHeaderTag
//
//import scala.collection.mutable.ArrayBuffer
//
///**
// * @author Kate Silverstein
// *         created on 2/16/15
// */
//object Utilities {
//  def makeSets(dir: String, trainPortion: Double = 0.75): Unit = {
//    import scala.util.Random
//    val files = new File(dir).listFiles
//    val docs = new ArrayBuffer[Document]
//    files.foreach(file => {
//      docs ++= LoadTSV(file.getAbsolutePath)
//    })
//    Random.shuffle(docs)
//    val trainDocs = docs.take((docs.length*trainPortion).floor.toInt)
//    val testDocs = docs.drop(trainDocs.length)
//    writeTSV(docsToTSV(trainDocs), "training.docs")
//    writeTSV(docsToTSV(testDocs), "testing.docs")
//  }
//  def docsToTSV(docs: Seq[Document]): Seq[String] = {
//    val tsvDocs = new ArrayBuffer[String]()
//    var ct = 0
//    docs.foreach(doc => {
//      tsvDocs += s"# $ct\n"
//      ct += 1
//      doc.sections.flatMap(_.tokens).foreach(token => {
//        val line = s"${token.attr[LabeledBioHeaderTag].categoryValue}\t${token.string}\n"
//        tsvDocs += line
//      })
//      tsvDocs += "\n"
//    })
//    tsvDocs.toSeq
//  }
//  def writeTSV(tsv: Seq[String], fname: String): Unit = {
//    val pw = new PrintWriter(new File(fname))
//    tsv.foreach(t => pw.write(t))
//    pw.close()
//  }
//  def main(args: Array[String]): Unit = {
//    val dataDir = System.getenv("PH_ROOT") + "/data"
//    makeSets(dataDir)
//  }
//}
