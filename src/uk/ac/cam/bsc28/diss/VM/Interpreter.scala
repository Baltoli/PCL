package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.VM.Types.Atom

class Interpreter(p: List[Instruction]) extends Runnable {

  val stack = new ArithmeticStack()

  // TODO: instruction sequence shouldn't be copied - instead
  // TODO: use a global interpreter manager that is responsible
  // TODO: for spawning off interpreters.
  val program = p

  /**
    * The interpreter environment maps _variables_ to either atoms
    * or integer values.
    */
  var environment = Map[Variable, Atom]()
  var labels = Map[String, Int]()

  var programCounter = 0

  var blocked: Option[Pair[Channel, Variable]] = None

  Threads.register(this)

  // TODO: check spawn semantics from the calculus
  def copy(): Interpreter = {
    val interpreter = new Interpreter(program)
    interpreter.environment = environment
    interpreter.labels = labels
    interpreter
  }

  def execute(i: Instruction): Unit = {
    i match {
      case stackOp : StackOperator =>
        stack operate stackOp

      case Load(v) =>
        environment get v foreach {
          case Left(c) =>
            println("Cannot dereference a channel. Ending execution.")
            programCounter = -1
          case Right(value) => stack push value
        }

      case Label(s) => () // Do nothing when we see a label - we've already extracted them,
                          // and removing them from the program is a lot of work.

      case Jump(s) =>
        programCounter = labels(s)

      case JumpIfZero(s) =>
        if((stack pop) == 0) programCounter = labels(s)

      case JumpIfNonZero(s) =>
        if((stack pop) != 0) programCounter = labels(s)

      case End() =>
        programCounter = -1

      case Spawn(s) =>
        val newInterpreter = copy()
        newInterpreter.programCounter = labels(s)
        Threads.runInNewThread { _ =>
          newInterpreter.run()
        }

      // In this case we don't need to look up anything in the
      // environment, so we can just directly notify the thread
      // manager with the atom being sent.
      case SendAtomDirect(chan, atom) =>
        Threads.notifyAll(chan, atom)

      // In this case we need to look up the environment for the
      // channel on which we are sending the atom. If we find an
      // appropriate entry in the environment then we can
      case SendAtomIndirect(channelVar, atom) =>
        environment get channelVar foreach {
          case Left(chan) => Threads.notifyAll(chan, atom)
          case _ => () // TODO: this should be a fatal error
        }

      // In this case we know the channel but not the data to be
      // sent - we look up the data and throw an error if no data
      // exists under the given variable name.
      case SendVariableDirect(chan, varName) =>
        environment get varName match {
          case Some(atom) =>
            Threads.notifyAll(chan, atom)
          case None =>
            () // TODO: fatal error
        }

      // This case just combines the two cases from above where we need
      // to look into the environment.
      case SendVariableIndirect(channelVar, varName) =>
        environment get varName match {
          case Some(atom) =>
            environment get channelVar foreach {
              case Left(chan) => Threads.notifyAll(chan, atom)
              case _ => () // TODO: fatal error
            }
          case None =>
            () // TODO: fatal error
        }

      case ReceiveDirect(c, n) =>
        () // TODO: implement

      case ReceiveIndirect(vc, n) =>
        () // TODO: implement (lookup in env.)

      case Read(n) =>
        val line = readLine("> ")
        try {
          val longValue = line.toLong
          environment += (n -> Right(longValue))
        } catch {
          case e: NumberFormatException => environment += (n -> Left(Channel(line)))
        }

      case Print() => println(stack.peek)

      case Let(vn, a) =>
        () // TODO: implement

      case Delete(vn) => environment -= vn
    }
  }

  def extractLabels(): Unit = {
    program.zipWithIndex foreach { p =>
      p._1 match {
        case Label(s) => labels += (s -> p._2)
        case _ => ()
      }
    }
  }

  def run(): Unit = {
    extractLabels()
    do {
      execute(program(programCounter))
      programCounter += 1
    } while (programCounter > 0)
  }

  def receive(c: Channel, v: Atom): Boolean = {
    blocked foreach (p =>
      if(c == p._1) {
        environment += (p._2 -> v)
        blocked = None
        this synchronized notify
        return true
      }
    )
    false
  }

}
