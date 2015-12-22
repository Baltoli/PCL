package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
       |out @c(1*4*5 + 2 + 3).in @stdio(X).end
    """.stripMargin
  val lexed = Lexer.tokenize(program)
  println(lexed)
  val p = new Parser(lexed)

  val result = p.parse()
  result match {
    case None => println("Parse error!")
    case Some(x) => println(x)
  }
}
