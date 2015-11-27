package uk.ac.cam.bsc28.diss.VM

abstract class Instruction
abstract class StackOperator extends Instruction

case class Push(v: Long) extends StackOperator
case class DereferencePush(s: String) extends Instruction
case class Add() extends StackOperator
case class Subtract() extends StackOperator
case class Multiply() extends StackOperator
case class Divide() extends StackOperator
case class CompareEqual() extends StackOperator
case class CompareZero() extends StackOperator
case class CompareNames(n: String, m: String) extends StackOperator

case class Label(s: String) extends Instruction
case class Jump(s: String) extends Instruction

case class End() extends Instruction
case class Spawn(s: String) extends Instruction

case class Receive(c: String, n: String) extends Instruction

case class SendInt(c: String, v: Int) extends Instruction // TODO
case class SendValue(c: String, n: String) extends Instruction // TODO

case class New[T](n: String, t: Class[T]) extends Instruction // TODO
case class Delete(n: String) extends Instruction // TODO

case class Read(n: String) extends Instruction // TODO
case class Print() extends Instruction