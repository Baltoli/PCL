package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.ParseTree._
import uk.ac.cam.bsc28.diss.VM.{Channel, SendAtomDirect, End, Instruction}

class CodeGenerator(prog: Program) {

  def generate(): List[Instruction] = {
    prog match {
      case Program(proc) => bytecodeForProcess(proc)
      case _ => List()
    }
  }

  def bytecodeForProcess(p: Process): List[Instruction] = {
    p match {
      case OutProcess(chan, expr) =>
        (chan, expr) match {
          case (ChannelName(cn), ChannelExpression(ChannelName(atom))) =>
            List(SendAtomDirect(Channel(cn), Left(Channel(atom))))
          case _ => List()
        }

      case InProcess(chan, varName) => List()
      case ParallelProcess(left, right) => List()
      case ReplicateProcess(proc) => List()
      case IfProcess(left, right, proc) => List()
      case LetProcess(name, value, proc) => List()

      case SequentialProcess(first, second) =>
        bytecodeForProcess(first) ++ bytecodeForProcess(second)

      case EndProcess() => List(End())
    }
  }

}
