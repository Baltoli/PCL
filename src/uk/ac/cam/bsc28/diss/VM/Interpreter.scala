package uk.ac.cam.bsc28.diss.VM

class Interpreter(p: List[Instruction]) extends Runnable {

  val stack = new ArithmeticStack()
  val program = p
  var environment = Map[String, Either[Channel, Long]]()
  var labels = Map[String, Int]()

  var programCounter = 0

  var blocked: Option[Pair[Channel, String]] = None

  Threads.register(this)

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

      case DereferencePush(n) =>
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

      case Receive(c, n) =>
        environment get c foreach {
            case Left(chan) =>
              blocked = Some(chan, n)
              this synchronized wait
            case _ =>
              println("Not receiving on a channel. Ending.")
              programCounter = -1
        }

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
