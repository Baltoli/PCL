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
    val i = new Interpreter(program)
    i.environment = environment
    i.labels = labels
    i
  }

  def execute(i: Instruction): Unit = {
    i match {
      case stackOp : StackOperator =>
        stack operate stackOp
      case Dereference(n) =>
        (environment get n) foreach { e =>
          e match {
            case Left(c) => println("Cannot dereference a channel.")
            case Right(v) => stack push v
          }
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
      case Receive(c, v) =>
        blocked = Some(c, v)
        this synchronized wait
      case Print() =>
        println(stack.peek)
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

  def receive(c: Channel, v: Either[Channel, Long]): Unit = {
    blocked foreach (p =>
      if(c == p._1) {
        environment += (p._2 -> v)
        blocked = None
        this synchronized notify
      }
    )
  }

}
