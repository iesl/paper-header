/**
 * Created by kate on 10/21/14.
 */

import edu.umass.cs.iesl.paperheader.crf._
import cc.factorie.app.nlp

object Test {
  def main(args: Array[String]): Unit = {
    println("hello paperheader")
    val docs = LoadTSV("/Users/kate/research/test-ph/data/fullpaper-headers.tsv", false).take(5)
    val tagger = HeaderTaggerCRF

    docs.foreach(doc => {
      val tokens = doc.sections.flatMap(_.tokens)
      tokens.foreach(token => {
        println(s"${token.string} ${token.attr[FormatInfo].toString()}")
      })
    })

  }

}
