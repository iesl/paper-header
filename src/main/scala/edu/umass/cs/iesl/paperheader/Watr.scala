package edu.umass.cs.iesl.paperheader

/**
  * Created by kate on 5/16/16.
  */

import java.io.{File, FileInputStream, InputStream}

import scala.io.Source

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.watr.watrmarks._

import scalaz.Scalaz._
import scalaz._

/* Watr/watr-marks notes */
//TODO dependency com.iheart#ficus_2.11;1.2.3: not found
//TODO add watr-marks as managed dependency, not fat jar --> sbt publishLocal watrmarks

/* paperheader notes */
//TODO add PageGeometry to doc.attr
//TODO seems to be many dups of first page? why
//TODO best way to integrate into pipeline?
//TODO figure out y-coord orientation
//TODO training data
//TODO watr test cases

//each pg has its own cartesian plane
//pageIter: give me pg 1;


object Watr {

  val LB = StandardLabels

  /**
   * Filename should be path to some [filename].json i.e. JSON representing a PDF generated using watr-works
   * @param filename path to some [filename].json
   * @return FACTORIE document with features
   */
  def fromFilename(filename: String): Document = {
    val ins = new FileInputStream(new File(filename))
    val doc = fromInputStream(ins)
    ins.close()
    doc
  }

  def fromInputStream(ins: InputStream): Document = {
    val loadPageIter = ZoneIterator.load(ins)
    val pageStream = unfold[Option[PageIterator], PageIterator](loadPageIter)(_ match {
      case Some(curr) => Some(curr, curr.nextPage)
      case None => None
    })

    val doc = new Document()

    pageStream.take(1).foreach { pageIter =>
      val zoneIter: Stream[ZoneIterator] = pageIter.getZones(LB.Block)
      zoneIter.foreach { thing =>
        println(thing)

      }

      //      zones.foreach { case (zone, zoneLabel) =>
      //        val tokens = zone.getTokens().map { tk => tk.value }
      //        println(tokens.mkString(","))
      //      }

    }

    doc
  }



  /* zone labels: met:bib-info , met:abstract , gen:body , line , met:title , met:author , gen:other , gen:references , token */

  def main(args: Array[String]): Unit = {
    val infilename = args(0)
    if (!new File(infilename).exists()) {
      throw new java.io.FileNotFoundException(infilename)
    }
    val doc = fromFilename(infilename)
    println(doc.tokenCount)
  }



}

