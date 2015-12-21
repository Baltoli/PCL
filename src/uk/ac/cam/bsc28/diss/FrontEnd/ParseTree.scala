package uk.ac.cam.bsc28.diss.FrontEnd

object ParseTree {

  // TODO: access modifiers for future usage

  trait Node
  case class Program(prog: Process) extends Node

  trait Start extends Node
  case class ProcessStart(proc: InternalProcess) extends Start

  trait Name extends Node
  case class VariableName(name: String) extends Name
  case class ChannelName(name: String) extends Name

  trait AddOperation extends Node
  case class AddNode() extends AddOperation
  case class SubtractNode() extends AddOperation

  trait MultiplyOperation extends Node
  case class MultiplyNode() extends MultiplyOperation
  case class DivideNode() extends MultiplyOperation

  trait Factor extends Node
  case class VariableFactor(n: VariableName) extends Factor
  case class LiteralFactor(v: Long) extends Factor

  trait Term extends Node
  case class FactorTerm(f: Factor) extends Term
  case class OpTerm(l: Factor, op: MultiplyOperation, r: Term) extends Term

  trait Expression extends Node
  case class TermExpression(t: Term) extends Expression
  case class OpExpression(l: Term, op: AddOperation, r: Expression) extends Expression
  case class ChannelExpression(c: ChannelName) extends Expression

  trait InternalProcess extends Node
  case class OutInternalProcess(chan: Name, expr: Expression, more: ProcessAux) extends InternalProcess
  case class InInternalProcess(chan: Name, varName: VariableName, more: ProcessAux) extends InternalProcess
  case class ParallelInternalProcess(left: InternalProcess, right: InternalProcess, more: ProcessAux) extends InternalProcess
  case class ReplicateInternalProcess(proc: InternalProcess, more: ProcessAux) extends InternalProcess
  case class IfInternalProcess(left: Expression, right: Expression, proc: InternalProcess, more: ProcessAux) extends InternalProcess
  case class LetInternalProcess(name: VariableName, value: Expression, proc: InternalProcess, more: ProcessAux) extends InternalProcess
  case class EndInternalProcess() extends InternalProcess
  case class SequentialInternalProcess(first: InternalProcess, second: InternalProcess) extends InternalProcess

  trait ProcessAux extends Node
  case class SequentialProcessAux(proc: InternalProcess, more: ProcessAux) extends ProcessAux
  case class EmptyProcessAux() extends ProcessAux

  trait Process extends Node
  case class OutProcess(chan: Name, expr: Expression) extends Process
  case class InProcess(chan: Name, varName: VariableName) extends Process
  case class ParallelProcess(left: Process, right: Process) extends Process
  case class ReplicateProcess(proc: Process) extends Process
  case class IfProcess(left: Expression, right: Expression, proc: Process) extends Process
  case class LetProcess(name: VariableName, value: Expression, proc: Process) extends Process
  case class SequentialProcess(first: Process, second: Process) extends Process
  case class EndProcess() extends Process

  def getAST(tree: Node): Option[Program] = {
    rebalance(tree) match {
      case ProcessStart(proc) => Some(Program(internalToExternal(proc)))
      case _ => None
    }
  }

  private def internalToExternal(tree: InternalProcess): Process = {
    tree match {
      case OutInternalProcess(chan, expr, more) => OutProcess(chan, expr)
      case InInternalProcess(chan, varName, more) => InProcess(chan, varName)
      case ParallelInternalProcess(left, right, more) =>
        ParallelProcess(internalToExternal(left), internalToExternal(right))
      case ReplicateInternalProcess(proc, more) =>
        ReplicateProcess(internalToExternal(proc))
      case IfInternalProcess(left, right, proc, more) =>
        IfProcess(left, right, internalToExternal(proc))
      case LetInternalProcess(name, value, proc, more) =>
        LetProcess(name, value, internalToExternal(proc))
      case EndInternalProcess() => EndProcess()
      case SequentialInternalProcess(first, second) =>
        SequentialProcess(internalToExternal(first), internalToExternal(second))
    }
  }

  private def rebalance(tree: Node): Node = {
    tree match {
      case ProcessStart(p) =>
        ProcessStart(rebalanceProcess(p))

      case _ => tree
    }
  }

  private def rebalanceProcess(tree: InternalProcess): InternalProcess = {
    val stripped = removeProcessAux(tree)
    tree match {
      case InInternalProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case OutInternalProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ParallelInternalProcess(_, _, aux) => sequenceIfNeeded(stripped, aux)
      case ReplicateInternalProcess(_, aux) => sequenceIfNeeded(stripped, aux)
      case IfInternalProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case LetInternalProcess(_, _, _, aux) => sequenceIfNeeded(stripped, aux)
      case _ => stripped
    }
  }

  private def sequenceIfNeeded(proc: InternalProcess, aux: ProcessAux) = {
    processFromAux(aux) match {
      case Some(next) => SequentialInternalProcess(proc, rebalanceProcess(next))
      case None => proc
    }
  }

  private def processFromAux(aux: ProcessAux): Option[InternalProcess] = {
    aux match {
      case SequentialProcessAux(proc, _) => Some(proc)
      case EmptyProcessAux() => None
    }
  }

  private def removeProcessAux(proc: InternalProcess) = {
    val empty = EmptyProcessAux()
    proc match {
      case InInternalProcess(c, v, _) => InInternalProcess(c, v, empty)
      case OutInternalProcess(c, e, _) => OutInternalProcess(c, e, empty)
      case ParallelInternalProcess(l, r, _) => ParallelInternalProcess(l, r, empty)
      case ReplicateInternalProcess(p, _) => ReplicateInternalProcess(p, empty)
      case IfInternalProcess(l, r, p, _) => IfInternalProcess(l, r, p, empty)
      case LetInternalProcess(n, v, p, _) => LetInternalProcess(n, v, p, empty)
      case _ => proc
    }
  }

}
