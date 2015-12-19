package uk.ac.cam.bsc28.diss.FrontEnd


object ParseTree {

  trait Node

  trait Start extends Node
  case class ProcessStart(proc: Process) extends Start

  trait Name extends Node
  case class VariableName(name: String) extends Name
  case class ChannelName(name: String) extends Name

  trait Operation extends Node
  case class AddNode() extends Operation
  case class MultiplyNode() extends Operation
  case class SubtractNode() extends Operation
  case class DivideNode() extends Operation

  trait Arithmetic extends Node
  case class IntegerWithAux(value: Long, more: ArithmeticAux) extends Arithmetic

  trait ArithmeticAux extends Node
  case class OperationArithmeticAux(op: Operation, right: Arithmetic, more: ArithmeticAux) extends ArithmeticAux
  case class EmptyArithmeticAux() extends ArithmeticAux

  trait Expression extends Node
  case class NameExpression(name: Name) extends Expression
  case class ArithmeticExpression(arith: Arithmetic) extends Expression

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
