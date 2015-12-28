package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.FrontEnd.ExternProcessor.ExternChannel
import uk.ac.cam.bsc28.diss.VM.Types.Atom

class Scheduler(prog: List[Instruction], externs: List[ExternChannel]) {

  private val loader = new ExternLoader()

  private val classes = externs map { e =>
    e.c -> loader.loadClassNamed(e.c)
  }

  def spawn(pc: Int, env: Map[Variable, Atom]): Unit = {
    val instances = classes.map { c =>
      c._1 -> loader.newInstance(c._2)
    }

    val interp = new Interpreter(prog, Map() ++ instances)
    interp.programCounter = pc
    interp.environment = env
    Scheduler.register(interp, this)

    Scheduler.runInNewThread { _ =>
      interp.run()
    }
  }

}

object Scheduler {

  var all = Map[Interpreter, Scheduler]()
  var semaphores = Map[String, (Interpreter, Int)]()

  def runInNewThread(f: Unit => Unit): Unit = {
    new Thread {
      override def run(): Unit = {
        f()
      }
    }.start()
  }

  def register(i: Interpreter, s: Scheduler): Unit = {
    all synchronized {
      all += (i -> s)
    }
  }

  def spawn(interp: Interpreter, pc: Int): Unit = {
    all.get(interp) match {
      case Some(s) => s.spawn(pc, interp.environment)
      case None => println("Bad bad unregistered interpreter.")
    }
  }

  def parallelGuard(interp: Interpreter, label: String, count: Int): Unit = {
    semaphores synchronized {
      semaphores += (label -> (interp, count))
    }
  }

  def threadDone(label: String): Unit = {
    semaphores synchronized {
      val maybe = semaphores.get(label)
      if (maybe.nonEmpty) {
        val (interp, count) = maybe.get
        if (count == 1) {
          interp synchronized interp.notifyAll()
        } else {
          semaphores += (label -> (interp, count - 1))
        }
      } else {
        println("Threads done on nonexistent label...") // TODO: error
      }
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
    while (!sent) {
      all.keys foreach { i =>
        if (!sent) {
          if (i receive(c, v)) sent = true
        }
      }
    }
  }

}
