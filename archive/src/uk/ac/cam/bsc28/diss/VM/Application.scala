package uk.ac.cam.bsc28.diss.VM

object Application extends App {
  val c = Channel("chan")
  val p = List(
  Spawn("a"),
  Spawn("b"),
  SendChannelDirect(Channel("chan"), Channel("")),
  End(),
  Label("a"),
  ReceiveDirect(Channel("chan"), Variable("x_a")),
  Push(10),
  End(),
  Label("b"),
  ReceiveDirect(Channel("chan"), Variable("x_b")),
  Push(20),
  End()
  )
  val i = new Interpreter(p, Map(), None, 0)
  i.run()
}
