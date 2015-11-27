package uk.ac.cam.bsc28.diss.VM

object Threads {
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

  def notifyAll(c: Channel, v: Either[Channel, Long]): Unit = {
    all synchronized {
      all foreach (_ receive(c, v))
    }
  }
}
