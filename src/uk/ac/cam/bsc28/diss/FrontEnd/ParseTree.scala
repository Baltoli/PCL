package uk.ac.cam.bsc28.diss.FrontEnd

object ParseTree {

  trait Node

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

  trait ProcessAux extends Node
  case class SequentialProcessAux(proc: InternalProcess, more: ProcessAux) extends ProcessAux
  case class EmptyProcessAux() extends ProcessAux

  trait Process extends ParseTree.Node
  case class OutProcess(chan: ParseTree.Name, expr: ParseTree.Expression) extends Process
  case class InProcess(chan: ParseTree.Name, varName: ParseTree.VariableName) extends Process
  case class ParallelProcess(left: Process, right: Process) extends Process
  case class ReplicateProcess(proc: Process) extends Process
  case class IfProcess(left: ParseTree.Expression, right: ParseTree.Expression, proc: Process) extends Process
  case class LetProcess(name: ParseTree.VariableName, value: ParseTree.Expression, proc: Process) extends Process
  case class SequentialProcess(first: Process, second: Process) extends Process
  case class EndProcess() extends Process
}
