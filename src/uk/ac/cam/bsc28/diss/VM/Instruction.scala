package uk.ac.cam.bsc28.diss.VM

abstract class Instruction

case class Push(v: Long) extends Instruction
case class Dereference(c: Channel) extends Instruction
case class Add() extends Instruction
case class Subtract() extends Instruction
case class Multiply() extends Instruction
case class Divide() extends Instruction
case class CompareEqual() extends Instruction
case class CompareZero() extends Instruction
case class CompareNames(n: Channel, m: Channel) extends Instruction
case class Label(s: String) extends Instruction
case class Jump(s: String) extends Instruction
case class End() extends Instruction
case class Spawn(s: String) extends Instruction
case class Receive(c: Channel, n: String) extends Instruction
case class Send[T](c: Channel, v: T) extends Instruction
case class New[T](n: String, t: Class[T]) extends Instruction
case class Delete(n: String) extends Instruction
case class Read[T](n: String, t: Class[T]) extends Instruction
case class Print[T](v: T) extends Instruction