package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.Config.Config
import uk.ac.cam.bsc28.diss.FrontEnd.ExternProcessor.ExternChannel
import uk.ac.cam.bsc28.diss.VM.Types.Atom
import java.util.concurrent.Executors.newCachedThreadPool

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class Scheduler(prog: List[Instruction], externs: List[ExternChannel]) {

  private val loader = new ExternLoader()

  private val classes = externs map { e =>
    e.c -> loader.loadClassNamed(e.c)
  }

  def spawn(pc: Int, env: Map[Variable, Atom], parent: Option[Interpreter]): Unit = {
    val instances = classes.map { c =>
      c._1 -> loader.newInstance(c._2)
    }

    val interp = new Interpreter(prog, Map() ++ instances, parent)
    interp.programCounter = pc
    interp.environment = env
    Scheduler.register(interp, this)

    Scheduler.runInNewThread(interp.run)
  }

}

object Scheduler {

  val all = TrieMap[Interpreter, Scheduler]()
  var semaphores = Map[String, (Interpreter, Int)]()

  def runInNewThread(f: () => Unit): Unit = {
    var tries = 1
    var success = false

    while (tries > 0) {
      try {
        val t = new Thread {
          override def run(): Unit = f()
        }
        t.start()

        success = true
        tries = 0
      } catch {
        case e: OutOfMemoryError =>
          if (Config.KILL_THREADS) {
            val blocks = new mutable.HashSet[(Channel, Variable)]()
            all foreach { i =>
              i._1.blocked match {
                case Some(b) =>
                  if (blocks.contains(b)) {
                    i._1.kill()
                  }
                  blocks += b

                case _ => ()
              }
            }
          } else {
            throw e
          }
      }

      tries -= 1
    }

    if (!success) {
      throw new OutOfMemoryError("No memory for threads.")
    }
  }

  def register(i: Interpreter, s: Scheduler): Unit = {
      all += (i -> s)
  }

  def spawn(interp: Interpreter, pc: Int): Unit = {
      all.get(interp) match {
        case Some(s) => s.spawn(pc, interp.environment, Some(interp))
        case None =>
          System.err.println(s"Bad bad unregistered interpreter: ${interp.hashCode()}")
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
