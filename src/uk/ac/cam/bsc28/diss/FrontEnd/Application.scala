package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
       |let X = 2*2 - 1 {
       |  out @stdio(X).
       |  (in @chan(Y).end | out @chan(X + 10))
       |}.
       |!(out @stdio(Y*6 / 2).end).
       |end
    """.stripMargin
  val lexed = Lexer.tokenize(program)
  println(lexed)
  val p = new Parser(lexed)

  val result = p.parse()
  result map { prog =>
    println(prog)
  }
}
