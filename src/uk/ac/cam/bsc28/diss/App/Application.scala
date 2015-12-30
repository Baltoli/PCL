package uk.ac.cam.bsc28.diss.App

import java.io.File

import uk.ac.cam.bsc28.diss.CodeGeneration.CodeGenerator
import uk.ac.cam.bsc28.diss.FrontEnd._
import uk.ac.cam.bsc28.diss.VM.{Scheduler}

object Application extends App {

  private final val usage = "usage: picl file"

  override def main(args: Array[String]): Unit = {

    if (args.length != 1) {
      println(usage)
      System.exit(1)
    }

    val maybeText = Filesystem.textFromTargetFileName(args(0))

    if (maybeText.isRight) {
      val err = maybeText.right.get
      println(s"File not found: $err")
      System.exit(2)
    }

    val programText = maybeText.left.get
    val (externs, source) = ExternProcessor.preprocessSource(programText)

    try {
      val tokens = Lexer.tokenize(source)
      val parser = new Parser(tokens)
      val tree = parser.parse()

      if (tree.nonEmpty) {
        val gen = new CodeGenerator(tree.get)
        val bytecode = gen.generate()

        val sched = new Scheduler(bytecode, externs)

        sched.spawn(0, Map())
    } else {
        println("Parsing error!")
        System.exit(3)
      }
    } catch {
      case e: LexException =>
        println(e.getMessage)
        System.exit(4)

      case e: ParseError =>
        println(e.getMessage)
        System.exit(5)
    }

  }

}
