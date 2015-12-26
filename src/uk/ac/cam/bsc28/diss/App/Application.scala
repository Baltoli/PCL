package uk.ac.cam.bsc28.diss.App

import uk.ac.cam.bsc28.diss.CodeGeneration.CodeGenerator
import uk.ac.cam.bsc28.diss.FrontEnd.{Parser, ExternProcessor, Lexer}
import uk.ac.cam.bsc28.diss.VM.{Scheduler, Interpreter}

object Application extends App {

  val prog =
    """
      |external @stdio
      |
      |let Pass = @password {
      | in @stdio(Chan).
      | [Chan = Pass] {
      |     out @stdio(@welcome).
      |     end
      | }.
      | out @stdio(@go_away)
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

    val sched = new Scheduler(bytecode, externs)

    sched.spawn(0)
  } else {
    println("Parsing error!")
  }

}
