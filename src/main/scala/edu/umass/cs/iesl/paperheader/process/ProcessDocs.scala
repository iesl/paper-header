package edu.umass.cs.iesl.paperheader.process

import cc.factorie.util.CmdOptions
import cc.factorie.app.nlp.Document
import edu.umass.cs.iesl.paperheader.tagger._


/**
 * Created by kate on 10/27/14.
 */

object Pipeline extends ChainedComponent {
  lazy val components = Seq(HeaderTaggerComponent)
}

class ProcessOpts extends CmdOptions{
  val dataFiles = new CmdOption("data-files", Nil.asInstanceOf[List[String]], "FILENAME...", "List of files to process (comma-separated).")
  val dataFile = new CmdOption("file", "", "STRING", "filename of data to process")
  val outFile = new CmdOption("out-file", "", "FILENAME...", "Output file.")
}

object DocProcessor {
  def apply(docs: Seq[Document]): Unit = {
    println(s"HeaderTagger processing ${docs.length} documents...")
    docs.foreach(doc => Pipeline.process1(doc))
    println("done.")
  }

  def main(args: Array[String]): Unit = {
    val opts = new ProcessOpts
    opts.parse(args)
    var docs: Seq[Document] = null
    if (opts.dataFiles.wasInvoked) {
      System.err.println("not yet implemented")
      System.exit(1)
    }
    if (opts.dataFile.wasInvoked) {
      docs = LoadTSV(opts.dataFile.value)
    }
    println(s"processing ${opts.dataFile.value}...")
    docs.foreach(Pipeline.process1)
    if (opts.outFile.wasInvoked) {
      println(s"writing to ${opts.outFile.value}...")
      writeToTSV(opts.outFile.value, docs)
    }
    println("done.")
  }

  /**
   * Write tagged docs to a tab-separated file with 4 columns: token_string, tag, gold_label, *_if_error
   * @param outputFilename
   * @param docs
   */
  def writeToTSV(outputFilename: String, docs: Seq[Document]): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(outputFilename))
    docs.foreach(doc => {
      pw.write("--DOCSTART--\n")
      doc.sections.flatMap(_.tokens).foreach(token => {
        val column0 = token.string //token string
        val column1 = token.attr[BioHeaderTag].categoryValue // tag
        val column2 = token.attr[LabeledBioHeaderTag].target.categoryValue // gold label
        val column3 = if (column1 == column2) " " else "*"
        pw.write(s"$column0\t$column1\t$column2\t$column3\n")
      })
    })
    pw.close()
  }
}
