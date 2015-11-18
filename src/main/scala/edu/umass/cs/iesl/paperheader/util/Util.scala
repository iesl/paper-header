package edu.umass.cs.iesl.paperheader.util

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.logging.{FileHandler, Logger}

/**
 * Created by kate on 11/17/15.
 */
object Util {

  def getLog(logname: String): Logger = {
    val resultsLog = Logger.getLogger(logname)
    try {
      val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd-mm-hh")
      val rightNow = LocalDateTime.now().format(fmt)
      val fh: FileHandler = new FileHandler(s"$rightNow-$logname.log")
      resultsLog.addHandler(fh)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    resultsLog
  }

}
