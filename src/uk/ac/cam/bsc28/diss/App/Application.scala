package uk.ac.cam.bsc28.diss.App

import uk.ac.cam.bsc28.diss.CodeGeneration.CodeGenerator
import uk.ac.cam.bsc28.diss.FrontEnd._
import uk.ac.cam.bsc28.diss.VM.{Scheduler}

trait AppMode
case object DumpIR extends AppMode

object Application extends App {

  private final val usage = "usage: picl file"

  override def main(args: Array[String]): Unit = {

    if (args.length != 1 && args.length != 2) {
      println(usage)
      System.exit(1)
    }

    val mode : Option[AppMode] = if (args.length == 2) {
      if (args(1) == "--dump") {
        Some(DumpIR)
      } else {
        println(s"Error: unrecognized mode selector (${args(1)})")
        System.exit(6)
        None
      }
    } else {
      None
    }

    def debugPrint(o: Any) = {
      mode match {
        case Some(DumpIR) => println(o)
        case _ => ()
      }
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
      debugPrint(s"Tokens:\n$tokens\n")

      val parser = new Parser(tokens)
      val tree = parser.parse()

      if (tree.nonEmpty) {
        debugPrint(s"Parse Tree:\n${tree.get}\n")
        val gen = new CodeGenerator(tree.get)
        val bytecode = gen.generate()

        debugPrint("Bytecode:")
        bytecode foreach debugPrint

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
