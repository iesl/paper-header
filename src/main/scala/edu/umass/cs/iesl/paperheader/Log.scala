package edu.umass.cs.iesl.paperheader

import java.util.logging.{FileHandler, Logger, SimpleFormatter}
/**
 * Created by kate on 1/26/16.
 */
//object Log {
//  var log: Logger = null
//  def apply(logfile: String): Unit = {
//    log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//    val handlers = log.getHandlers
//    val fh = new FileHandler(logfile)
//    val fmt = new SimpleFormatter()
//    fh.setFormatter(fmt)
//    log.addHandler(fh)
//  }
//}

class Log(filename: String) {
  val log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
  init()
  def init(): Unit = {
    val handlers = log.getHandlers
    val fh = new FileHandler(filename)
    val fmt = new SimpleFormatter()
    fh.setFormatter(fmt)
    log.addHandler(fh)
  }
}