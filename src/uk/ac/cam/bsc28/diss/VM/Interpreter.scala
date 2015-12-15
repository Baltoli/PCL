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

  Scheduler.register(this)

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
        Scheduler.runInNewThread { _ =>
          newInterpreter.run()
        }

      // In this case we don't need to look up anything in the
      // environment, so we can just directly notify the thread
      // manager with the atom being sent.
      case SendAtomDirect(chan, atom) =>
        Scheduler.notifyAll(chan, atom)

      // In this case we need to look up the environment for the
      // channel on which we are sending the atom. If we find an
      // appropriate entry in the environment then we can notify
      // a thread waiting on that channel.
      //
      // We use the nested match Some(Left(...)) as a shortcut -
      // the only valid case for getting the channel is when we
      // have an entry in the environment (Some) and it is a channel
      // (Left).
      case SendAtomIndirect(channelVar, atom) =>
        environment get channelVar match {
          case Some(Left(chan)) => Scheduler.notifyAll(chan, atom)
          case _ => () // TODO: this should be a fatal error
        }

      // In this case we know the channel but not the data to be
      // sent - we look up the data and throw an error if no data
      // exists under the given variable name. No need for nested
      // matching here as we aren't breaking the Either down any
      // further.
      case SendVariableDirect(chan, varName) =>
        environment get varName match {
          case Some(atom) =>
            Scheduler.notifyAll(chan, atom)
          case None =>
            () // TODO: fatal error
        }

      // This case just combines the two cases from above where we need
      // to look into the environment. Same use of nested matching as
      // before for the channel variable.
      case SendVariableIndirect(channelVar, varName) =>
        environment get varName match {
          case Some(atom) =>
            environment get channelVar match {
              case Some(Left(chan)) => Scheduler.notifyAll(chan, atom)
              case _ => () // TODO: fatal error
            }
          case None =>
            () // TODO: fatal error
        }

      // In this case we know the channel and the variable name that will
      // form a blocking pair, so we can just update `blocked` and call
      // a synchronized wait.
      case ReceiveDirect(c, n) =>
        () // TODO: implement

      // Similar to above, but we need to also look up the channel stored
      // in the variable before we can block on it. Fatal error if no such
      // variable is in the env OR if it's holding an int.
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
