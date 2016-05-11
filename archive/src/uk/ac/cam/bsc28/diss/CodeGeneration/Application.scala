package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.{ExternProcessor, ParseTree, Parser, Lexer}
import uk.ac.cam.bsc28.diss.VM.Interpreter

object Application extends App {

  val prog =
    """
       |external @stdio
       |
       |let X = 0 {
       |  [@c = X] {
       |    out @stdio(0)
       |  }.
       |  ( out @c(0) | in @c(X) )
       |}
    """.stripMargin

}
