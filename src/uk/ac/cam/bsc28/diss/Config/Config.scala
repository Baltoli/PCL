package uk.ac.cam.bsc28.diss.Config

import java.io.PrintStream

object Config {

  var DUMP_IR = false
  var TRACE = false

  def configPrint(stream: PrintStream, cond: => Boolean)(o: Any) = {
    if (cond) {
      stream.println(o)
    }
  }

  val dumpLine = configPrint(System.err, DUMP_IR) _
  val traceLine = configPrint(System.err, TRACE) _

}
