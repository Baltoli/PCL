package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.{ParseTree, Parser, Lexer}

object Application extends App {

  val prog =
    """
      |out @stdio(@c).
      |out @stdio(1).
      |out @stdio(Var).
      |out Chan(@c).
      |out Chan(1).
      |out Chan(Var).
      |in @stdio(X).
      |in Chan(Y).
      |end
    """.stripMargin

  val tokens = Lexer.tokenize(prog)
  val parser = new Parser(tokens)
  val tree = parser.parse()

  if (tree.nonEmpty) {
    println(tree.get)
    val gen = new CodeGenerator(tree.get)
    println(gen.generate())
  } else {
    println("Parsing error!")
  }

}
