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

  // This notification method performs a unicast-like operation
  // to send an atom to an interpreter. Nondeterminism is handled
  // by sending to the first registered interpreter that is blocked
  // on the channel c.
  // Possible extensions here include implementing a multicast
  // version that sends to all available channels instead of just one
  // (as the pi-calculus reduction semantics are unicast). Need to write
  // about the nondeterminism strategy used in the report (i.e. how
  // the choice of strategy can affect execution of programs).
  def notifyAll(c: Channel, v: Atom): Unit = {
    var sent = false
    all foreach { i =>
      if (!sent) {
        if (i receive(c, v)) sent = true
      }
    }
  }
}
