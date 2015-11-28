package uk.ac.cam.bsc28.diss.VM

object Application extends App {
  val c = Channel("chan")
  val p = List(
  Push(0),
  JumpIfNonZero("print"),
  End(),
  Label("print"),
  Push(12),
  Print(),
  End()
  )
  val i = new Interpreter(p)
  i.run()
}
