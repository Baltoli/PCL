package uk.ac.cam.bsc28.diss.VM


object Application extends App {
  val c = Channel("chan")
  val p = List(
    Spawn("p"),
    Receive("n", "v"),
    End(),
    Label("p"),
    SendInt("n", 0),
    End()
  )
  val i = new Interpreter(p)
  i.run()
}
