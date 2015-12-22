package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.{ParseTree, Parser, Lexer}
import uk.ac.cam.bsc28.diss.VM.Interpreter

object Application extends App {

  val prog =
    """
      |(
      | in @x(A) |
      | out @x(@y)
      |).
      |end
    """.stripMargin

  val tokens = Lexer.tokenize(prog)
  val parser = new Parser(tokens)
  val tree = parser.parse()

  if (tree.nonEmpty) {
    println(tree.get)
    val gen = new CodeGenerator(tree.get)
    val prog = gen.generate()
    println(prog)

    val interp = new Interpreter(prog)
    interp.run()
  } else {
    println("Parsing error!")
  }

}
