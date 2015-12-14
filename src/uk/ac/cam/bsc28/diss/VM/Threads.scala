package uk.ac.cam.bsc28.diss.VM

object Threads { // TODO: Scheduler
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

  // TODO: broadcast vs. unicast!!!
  // TODO: pi-calculus semantics specify unicast but broadcast may be a useful extension
  // TODO: further investigation here
  // TODO: nondeterminism strategy
  def notifyAll(c: Channel, v: Either[Channel, Long]): Unit = {
    var end = false
    while(!end) {
      all foreach { i =>
        val sent = i.receive(c, v)
        if(sent) end = true
      }
    }
  }
}
