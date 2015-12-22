package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.ParseTree._
import uk.ac.cam.bsc28.diss.VM._

class CodeGenerator(prog: Program) {

  def generate(): List[Instruction] = {
    prog match {
      case Program(proc) => bytecodeForProcess(proc)
      case _ => List()
    }
  }

  def bytecodeForProcess(p: Process): List[Instruction] = {
    p match {
      case OutProcess(name, expr) =>
        (name, expr) match {
          case (ChannelName(cn), ChannelExpression(ChannelName(data))) =>
            List(SendChannelDirect(Channel(cn), Channel(data)))

          case (VariableName(vn), ChannelExpression(ChannelName(data))) =>
            List(SendChannelIndirect(Variable(vn), Channel(data)))

          case (ChannelName(cn), TermExpression(FactorTerm(VariableFactor(VariableName(data))))) =>
            List(SendVariableDirect(Channel(cn), Variable(data)))

          case (VariableName(vn), TermExpression(FactorTerm(VariableFactor(VariableName(data))))) =>
            println(expr)
            List(SendVariableIndirect(Variable(vn), Variable(data)))

          case (ChannelName(cn), e) =>
            bytecodeForExpression(e) ++ List(SendIntDirect(Channel(cn)))

          case (VariableName(vn), e) =>
            bytecodeForExpression(e) ++ List(SendIntIndirect(Variable(vn)))
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

  def bytecodeForExpression(e: Expression): List[Instruction] = {
    e match {
      case TermExpression(t) =>
        bytecodeForTerm(t)

      case OpExpression(l, op, r) =>
        val lCode = bytecodeForTerm(l)
        val operatorCode = List(op match {
          case AddNode() => Add()
          case SubtractNode() => Subtract()
        })
        val rCode = bytecodeForExpression(r)
        lCode ++ operatorCode ++ rCode

      case ChannelExpression(c) => List() // TODO: this is an error
    }
  }

  def bytecodeForTerm(t: Term): List[Instruction] = {
    t match {
      case FactorTerm(f) =>
        bytecodeForFactor(f)

      case OpTerm(l, op, r) =>
        val lCode = bytecodeForFactor(l)
        val operatorCode = List(op match {
          case MultiplyNode() => Multiply()
          case DivideNode() => Divide()
        })
        val rCode = bytecodeForTerm(r)
        lCode ++ operatorCode ++ rCode
    }
  }

  def bytecodeForFactor(f: Factor): List[Instruction] = {
    f match {
      case VariableFactor(VariableName(vn)) =>
        List(Load(Variable(vn)))

      case LiteralFactor(v) =>
        List(Push(v))

      case _ => List() // TODO: error
    }
  }

}
