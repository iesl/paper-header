package edu.umass.cs.iesl.paperheader

/**
  * Created by kate on 5/16/16.
  */

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.io.{File, FileInputStream, InputStream}

import edu.umass.cs.iesl.watr.watrmarks._
import edu.umass.cs.iesl.watr.watrmarks.ZoneIterator._

import scala.collection.mutable.{HashMap, ArrayBuffer}

import cc.factorie.app.nlp._

/* Watr/watr-marks notes */
//TODO dependency com.iheart#ficus_2.11;1.2.3: not found
//TODO add watr-marks as managed dependency, not fat jar

/* paperheader notes */
//TODO add PageGeometry to doc.attr
//TODO seems to be many dups of first page? why
//TODO best way to integrate into pipeline?
//TODO figure out y-coord orientation
//TODO training data


object Watr {

  def fromFilename(filename: String): Document = {
    //TODO two FileInputStreams? do I need to get both PageIterator and ZoneIndexer?
    val json = Json.parse(new FileInputStream(filename))
    val pgIter: Option[PageIterator] = ZoneIterator.load(new FileInputStream(filename))
    val zixer: ZoneIndexer = loadSpatialIndices(json)
    val doc = new Document("")

    pgIter match {
      case Some(pageIter) =>
        val pageID = pageIter.currPageID
        val spatialIx = zixer.pageRIndexes(pageID)  // page to region ixes?

        /* get bounding box for page #0? */
        val rect = spatialIx.getBounds()
        val bounds = jsiRectangle.toLTBounds(rect)

        /* get all zones contained in page #0 bounding box */
        val pageZones = zixer.query(pageID, bounds)

        pageZones.foreach { pageZone =>
          /* get all zones in page box with label 'line' */
          val lineZoneLabels = zixer.getZoneLabels(pageZone.id).filter(zoneLabel => zoneLabel.key.equals("line"))

          /* for each of these 'line zones', do */
          lineZoneLabels.foreach { lineZoneLabel =>

            /* get the zones within this line from the PageIterator ? */
            val lineZones = pageIter.getZones(lineZoneLabel)

            /* for each zone within this line, do */
            lineZones.foreach { lineZoneIter =>
              /* get the tokens i.e. (Zone, Label) pairs */
              val toks = lineZoneIter.getTokens()
              /* add each token (with corresponding bbox info in tok.attr) to a FACTORIE doc */
              toks.foreach { case (tkzone, tklabel) =>
                /* bbox = TargetedBounds */
                val bbox = tkzone.bboxes.head //TODO only take head ??
              /* box = LTBounds */
              val box = bbox.bbox  //LTBounds
                tklabel.value match {
                  case Some(tokenString) =>
                    val token = new Token(doc, tokenString)
                    token.attr += box
                  case _ =>
                }
              }
            }
          }
        }
      case _ =>
    }
    doc
  }

  //  implicit object ZoneRecordsRead extends Reads[ZoneRecords]
  def loadSpatialIndices(jsvalue: JsValue): ZoneIndexer = {
    jsvalue.validate[ZoneRecords] match {
      case JsSuccess(zoneRecords, path) =>
        ZoneIndexer.loadSpatialIndices(zoneRecords)
      case JsError(err) =>
        sys.error(s"error validating zone records: ${err}")
    }
  }


  /* zone labels: met:bib-info , met:abstract , gen:body , line , met:title , met:author , gen:other , gen:references , token */

  def main(args: Array[String]): Unit = {
    val infilename = "/home/kate/paper-header/paper.json"
    val doc = fromFilename(infilename)
    println(doc.tokenCount)
//    val infile = new File(infilename)
//    val pgIter: Option[PageIterator] = ZoneIterator.load(new FileInputStream(infile))
//    val json = Json.parse(new FileInputStream(infile))
//    val zixer: ZoneIndexer = loadSpatialIndices(json)
//    val doc = new Document("")
//    //TODO add PageGeometry to doc.attr
//    //TODO seems to be many dups of first page? why
//    //TODO watrmark dep rather than unmanaged fat-jar?
//    //TODO best way to integrate into pipeline?
//    //TODO figure out y-coord orientation
//    //TODO train!
//
//    pgIter match {
//      case Some(pageIter) =>
//        val pageID = pageIter.currPageID
//        val spatialIx = zixer.pageRIndexes(pageID)  // page to region ixes?
//
//        /* get bounding box for page #0? */
//        val rect = spatialIx.getBounds()
//        val bounds = jsiRectangle.toLTBounds(rect)
//
//        /* get all zones contained in page #0 bounding box */
//        val pageZones = zixer.query(pageID, bounds)
//
//        pageZones.foreach { pageZone =>
//          /* get all zones in page box with label 'line' */
//          val lineZoneLabels = zixer.getZoneLabels(pageZone.id).filter(zoneLabel => zoneLabel.key.equals("line"))
//
//          /* for each of these 'line zones', do */
//          lineZoneLabels.foreach { lineZoneLabel =>
//
//            /* get the zones within this line from the PageIterator ? */
//            val lineZones = pageIter.getZones(lineZoneLabel)
//
//            /* for each zone within this line, do */
//            lineZones.foreach { lineZoneIter =>
//              /* get the tokens i.e. (Zone, Label) pairs */
//              val toks = lineZoneIter.getTokens()
//              /* add each token (with corresponding bbox info in tok.attr) to a FACTORIE doc */
//              toks.foreach { case (tkzone, tklabel) =>
//                /* bbox = TargetedBounds */
//                val bbox = tkzone.bboxes.head //TODO only take head ??
//                /* box = LTBounds */
//                val box = bbox.bbox  //LTBounds
//                tklabel.value match {
//                  case Some(str) =>
//                    val token = new Token(doc, str)
//                    token.attr += box
//                  case _ =>
//                }
//              }
//            }
//          }
//        }
//      case _ =>
//    }
//
//    doc.tokens.foreach { tok =>
//      println(s"${tok.string} ${tok.attr.toString()}")
//    }
//    print(doc.tokenCount)
  }


//  def zix2text(zixer: ZoneIndexer): String = {
//    val tokenStrings = zixer.zoneLabelMap.flatMap { case (zoneIx, labels) =>
//      labels.head.key match {
//        case "token" => labels.head.value
//        case _ => None
//      }
//    }
//    tokenStrings.mkString("\n")
//  }
//
//  def traverseZoneIter(ziter: ZoneIterator): Seq[String] = {
//    val buff = new ArrayBuffer[String]()
//    var curr = ziter.next
//    var done = false
//    while (!done) {
//      curr match {
//        case Some(zi) =>
//          buff += zi.getText()
//          curr = zi.next
//          done = true
//        case _ => done = true
//      }
//    }
//    print("DONE")
//    buff
//  }

}

