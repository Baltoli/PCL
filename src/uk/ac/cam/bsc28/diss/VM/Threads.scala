package uk.ac.cam.bsc28.diss.VM

object Threads {
  var all = List[Interpreter]()

  def notifyAll(c: Channel, v: Either[Channel, Long]): Unit = {
    all synchronized {
      all map (_ receive(c, v))
    }
  }
}
