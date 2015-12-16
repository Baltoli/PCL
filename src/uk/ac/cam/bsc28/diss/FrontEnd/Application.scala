package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
      |let X = @stdio {
      |   out X(0).
      |   end
      |}
    """.stripMargin
  val lexed = Lexer.tokenize(program)
  print(lexed)
}
