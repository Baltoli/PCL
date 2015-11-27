package uk.ac.cam.bsc28.diss.VM

object Application extends App {
  val p = List( Spawn("m"),
                End(),
                Label("n"),
                Push(3),
                Push(4),
                Add(),
                Print(),
                End(),
                Label("m"),
                Spawn("n"),
                Push(8),
                Push(2),
                Multiply(),
                Push(2),
                Add(),
                Print(),
                End())
  val i = new Interpreter(p)
  i.run()
}
