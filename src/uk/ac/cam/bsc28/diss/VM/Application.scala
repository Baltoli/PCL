package uk.ac.cam.bsc28.diss.VM

object Application extends App {
  val c = Channel("chan")
  val p = List(
  Let(Variable("x"), Right(0)),
  Load(Variable("x")),
  Print(),
  Delete(Variable("x")),
  Read(Variable("i")),
  Read(Variable("c")),
  Spawn("other"),
  SendVariableIndirect(Variable("c"), Variable("i")),
  End(),
  Label("other"),
  ReceiveDirect(Channel("chan"), Variable("n")),
  Load(Variable("n")),
  Print(),
  End()
  )
  val i = new Interpreter(p)
  i.run()
}
