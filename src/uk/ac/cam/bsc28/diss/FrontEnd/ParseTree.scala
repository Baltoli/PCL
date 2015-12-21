package uk.ac.cam.bsc28.diss.FrontEnd


object ParseTree {

  trait Node

  trait Start extends Node
  case class ProcessStart(proc: Process) extends Start

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

  trait Process extends Node
  case class OutProcess(chan: Name, expr: Expression, more: ProcessAux) extends Process
  case class InProcess(chan: Name, varName: VariableName, more: ProcessAux) extends Process
  case class ParallelProcess(left: Process, right: Process, more: ProcessAux) extends Process
  case class ReplicateProcess(proc: Process, more: ProcessAux) extends Process
  case class IfProcess(left: Expression, right: Expression, proc: Process, more: ProcessAux) extends Process
  case class LetProcess(name: VariableName, value: Expression, proc: Process, more: ProcessAux) extends Process
  case class EndProcess() extends Process

  trait ProcessAux extends Node
  case class SequentialProcessAux(proc: Process, more: ProcessAux) extends ProcessAux
  case class EmptyProcessAux() extends ProcessAux

}
