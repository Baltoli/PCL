package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.{ExternProcessor, ParseTree, Parser, Lexer}
import uk.ac.cam.bsc28.diss.VM.Interpreter

object Application extends App {

  val prog =
    """
       |external @stdio
       |
       |let X = @c {
       |  [@c = X] {
       |    out @stdio(0)
       |  }.
       |  ( out @c(0) | in @c(X) )
       |}
    """.stripMargin

  val (externs, source) = ExternProcessor.preprocessSource(prog)
  val tokens = Lexer.tokenize(source)
  val parser = new Parser(tokens)
  val tree = parser.parse()

  if (tree.nonEmpty) {
    println(tree.get)
    val gen = new CodeGenerator(tree.get)
    val bytecode = gen.generate()
    println(bytecode)

    val interp = new Interpreter(bytecode, externs)
    interp.run()
  } else {
    println("Parsing error!")
  }

}
