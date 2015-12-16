package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.VM.Types.Atom

object Scheduler {
  var all = List[Interpreter]()

  def runInNewThread(f: Unit => Unit): Unit = {
    new Thread {
      override def run(): Unit = {
        f()
      }
    }.start()
  }

  def register(i: Interpreter): Unit = {
    all synchronized {
      all ::= i
    }
  }

  def notifyAll(c: Channel, v: Atom): Unit = {
    var sent = false
    all foreach { i =>
      if (!sent) {
        if (i receive(c, v)) sent = true
      }
    }
  }
}
