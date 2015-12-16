package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
      |in @stdio(X).
      |out @some_chan(XYZ).
      |end.
    """.stripMargin
  val lexed = Lexer.tokenize(program)
  print(lexed)
}
