package edu.umass.cs.iesl.paperheader.process

import cc.factorie.util.CmdOptions
import cc.factorie.app.nlp.Document


/**
 * Created by kate on 10/27/14.
 */

object Pipeline extends ChainedComponent {
  lazy val components = Seq(HeaderTaggerComponent)
}

class ProcessOpts extends CmdOptions{
  val dataFiles = new CmdOption("data-files", Nil.asInstanceOf[List[String]], "FILENAME...", "List of files to process (comma-separated).")
  val outFile = new CmdOption("out-file", "", "FILENAME...", "Output file.")
}

object DocProcessor {
  def apply(docs: Seq[Document]): Unit = {
    docs.foreach(doc => Pipeline.process1(doc))
  }
}
