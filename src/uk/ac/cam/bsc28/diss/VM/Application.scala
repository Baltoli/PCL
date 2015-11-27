package uk.ac.cam.bsc28.diss.VM

object Application extends App {
  val c = Channel("chan")
  val p = List(
    NewChannel("n"),
    Spawn("p"),
    Receive("n", "v"),
    DereferencePush("v"),
    Print(),
    End(),
    Label("p"),
    Read("m"),
    SendValue("n", "m"),
    Delete("n"),
    End()
  )
  val i = new Interpreter(p)
  i.run()
}
