package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
       |in @stdio(Z).
       |out Z(1 + 2*3).
       |end
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
