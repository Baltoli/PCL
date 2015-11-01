package uk.ac.cam.bsc28.diss.VM

class Interpreter(p: List[Instruction]) extends Runnable {

  val stack = new ArithmeticStack()
  val program = p
  var environment = Map[String, Either[Channel, Long]]()

  var blocked: Option[Pair[Channel, String]] = None

  def execute(i: Instruction): Unit = {
    i match {
      case stackOp : StackOperator =>
        stack operate stackOp
      case Dereference(n) =>
        environment get n map {
          case Left(c) => println("Cannot dereference a channel.")
          case Right(v) => stack push v
        }
      case Receive(c, v) =>
        blocked = Some(c, v)
        this synchronized wait
    }
  }

  def run(): Unit = {
    program map execute
  }

  def receive(c: Channel, v: Either[Channel, Long]): Unit = {
    blocked map (p =>
      if(c == p._1) {
        environment += (p._2 -> v)
        blocked = None
        this synchronized notify
      }
    )
  }

}
