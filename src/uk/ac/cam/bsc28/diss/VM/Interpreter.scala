package uk.ac.cam.bsc28.diss.VM

class Interpreter(p: List[Instruction]) extends Runnable {

  val stack = new ArithmeticStack()

  // TODO: instruction sequence shouldn't be copied - instead
  // TODO: use a global interpreter manager that is responsible
  // TODO: for spawning off interpreters.
  val program = p

  /**
    * The interpreter environment maps _variables_ to either atoms
    * or integer values.
    *
    * TODO: Instead of either, use an alias / custom type here.
    */
  var environment = Map[String, Either[Channel, Long]]()
  var labels = Map[String, Int]()

  var programCounter = 0

  var blocked: Option[Pair[Channel, String]] = None

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

      case Load(n) =>
        environment get n foreach {
          case Left(c) =>
            println("Cannot dereference a channel. Ending execution.")
            programCounter = -1
          case Right(v) => stack push v
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

      case SendInt(c, v) =>
        environment get c foreach {
          case Left(chan) => Threads.notifyAll(chan, Right(v))
          case _ => ()
        }

      case SendValue(c, n) =>
        environment get c foreach {
          case Left(chan) =>
            environment get n foreach { e =>
              Threads.notifyAll(chan, e)
            }
          case _ => ()
        }

      case ReceiveDirect(c, n) =>
        () // TODO: implement

      case ReceiveIndirect(vc, n) =>
        () // TODO: implement

      case Read(n) =>
        val line = readLine("> ")
        try {
          val longValue = line.toLong
          environment += (n -> Right(longValue))
        } catch {
          case e: NumberFormatException => environment += (n -> Left(Channel(line)))
        }

      case Print() => println(stack.peek)

      case NewInt(n) => environment += (n -> Right(0))

      case NewChannel(n) => environment += (n -> Left(Channel(n)))

      case Delete(n) => environment -= n
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

  def receive(c: Channel, v: Either[Channel, Long]): Boolean = {
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
