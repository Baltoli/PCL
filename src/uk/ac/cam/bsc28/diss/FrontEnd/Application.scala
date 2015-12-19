package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
       |let Y = 0 {
       |  (in @stdio(X) | out @stdio(Y)).
       |  in @stdio(Z).
       |  out Z(Y).
       |  end
       |}
    """.stripMargin
  val lexed = Lexer.tokenize(program)
  println(lexed)
  val p = new Parser(lexed)

  val result = p.parse()
  result match {
    case Left(t) => println(t)
    case Right(e) => println(e.err)
  }
}
