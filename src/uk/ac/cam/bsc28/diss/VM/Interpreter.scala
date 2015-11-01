package uk.ac.cam.bsc28.diss.VM

class Interpreter(p: List[Instruction]) extends Runnable {

  val stack = new ArithmeticStack()
  val program = p

  def execute(i: Instruction): Unit = {
    i match {
      case stackOp : StackOperator =>
        stack operate stackOp
    }
  }

  def run(): Unit = {
    program map execute
  }

}
