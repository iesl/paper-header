package edu.umass.cs.iesl.paperheader.process

import cc.factorie.util.CmdOptions
import cc.factorie.app.nlp._
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain}
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
  val extractFirstLastNames = new CmdOption("firstlast", false, "BOOLEAN", "extract first/last names as postprocessing step")
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
    FormatData.calculateMaxDims(docs)
    docs.foreach(Pipeline.process1)
    if (opts.outFile.wasInvoked) {
      if (opts.extractFirstLastNames.value) {
        firstLastNames(docs)
//        writeToTSV(opts.outFile.value, docs, firstLast=true)
      } else {
        println(s"writing to ${opts.outFile.value}...")
        writeToTSV(opts.outFile.value, docs)
      }
    }
    println("done.")
  }

  object LexiconDomain extends CategoricalVectorDomain[String]
  class LexiconTag(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = LexiconDomain
    override def skipNonCategories = true
  }

  def firstLastNames(docs: Seq[Document]): Unit = {
    val vf = (t:Token) => t.attr[LexiconTag]
    docs.take(2).foreach(doc => {
      val tokens = doc.sections.flatMap(_.tokens).toSeq.filter(_.attr[BioHeaderTag].categoryValue.substring(2) == "author")
      tokens.foreach(t => {
        t.attr += new LexiconTag(t)
      })
      lexicon.iesl.PersonFirst.tagText(tokens,vf,"PERSON-FIRST")
      lexicon.iesl.PersonFirstHigh.tagText(tokens,vf,"PERSON-FIRST-HIGH")
      lexicon.iesl.PersonFirstHighest.tagText(tokens,vf,"PERSON-FIRST-HIGHEST")
      lexicon.iesl.PersonFirstMedium.tagText(tokens,vf,"PERSON-FIRST-MEDIUM")
      lexicon.iesl.PersonLast.tagText(tokens,vf,"PERSON-LAST")
      lexicon.iesl.PersonLastHigh.tagText(tokens,vf,"PERSON-LAST-HIGH")
      lexicon.iesl.PersonLastHighest.tagText(tokens,vf,"PERSON-LAST-HIGHEST")
      lexicon.iesl.PersonLastMedium.tagText(tokens,vf,"PERSON-LAST-MEDIUM")
      lexicon.iesl.PersonHonorific.tagText(tokens,vf,"PERSON-HONORIFIC")
      tokens.foreach(t => {
        vf(t).activeCategories.foreach(println)
        val tags = vf(t).activeCategories
        val first = Set("PERSON-FIRST", "PERSON-FIRST-HIGH", "PERSON-FIRST-HIGHEST", "PERSON-FIRST-MEDIUM")
        val last = Set("PERSON-LAST", "PERSON-LAST-HIGH", "PERSON-LAST-HIGHEST", "PERSON-LAST-MEDIUM")
        val labels = cc.factorie.util.JavaHashMap[String, Int]()
        labels("author-firstname") = 0
        labels("author-lastname") = 0
        tags.foreach(tag => {
          if (first.contains(tag)) labels("author-firstname") += 1
          if (last.contains(tag)) labels("author-lastname") += 1
        })
        if (labels.values.sum > 0) {
          if (labels("author-firstname") > labels("author-lastname")) t.attr += new BioHeaderTag2(t, "author-firstname")
          else if (labels("author-firstname") > labels("author-lastname")) t.attr += new BioHeaderTag2(t, "author-lastname")
          else t.attr += new BioHeaderTag2(t, "author-misc")
        }
      })
    })
  }

  /**
   * Write tagged docs to a tab-separated file with 4 columns: token_string, tag, gold_label, *_if_error
   * @param outputFilename
   * @param docs
   */
  def writeToTSV(outputFilename: String, docs: Seq[Document], firstLast:Boolean = false): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(outputFilename))
    docs.foreach(doc => {
      pw.write("--DOCSTART--\n")
      doc.sections.flatMap(_.tokens).foreach(token => {
        val column0 = token.string //token string
        val column1 = firstLast match {
            case true => token.attr[BioHeaderTag2].categoryValue
            case _ => token.attr[BioHeaderTag].categoryValue
          }
        val column2 = firstLast match {
          case true => token.attr[LabeledBioHeaderTag2].categoryValue
          case _ => token.attr[LabeledBioHeaderTag].categoryValue
        }
        val column3 = if (column1 == column2) " " else "*"
        pw.write(s"$column0\t$column1\t$column2\t$column3\n")
      })
    })
    pw.close()
  }
}
