package uk.ac.cam.bsc28.diss.VM

// TODO: name review

abstract class Instruction
abstract class StackOperator extends Instruction

case class Push(v: Long) extends StackOperator
case class Load(s: String) extends Instruction // TODO: better names
case class Add() extends StackOperator
case class Subtract() extends StackOperator
case class Multiply() extends StackOperator
case class Divide() extends StackOperator
case class CompareEqual() extends StackOperator
case class CompareZero() extends StackOperator
case class CompareNames(n: String, m: String) extends StackOperator // TODO: rethink this. Resolve name/variable ambiguity through syntax
// TODO: read into erlang atoms / ruby symbols

case class Label(s: String) extends Instruction
case class Jump(s: String) extends Instruction
case class JumpIfZero(s: String) extends Instruction
case class JumpIfNonZero(s: String) extends Instruction

case class End() extends Instruction
case class Spawn(s: String) extends Instruction

// TODO: syntactic classes separate here for variables and names
case class Receive(c: String, n: String) extends Instruction

// TODO: consider sending nondeterminism - erlang model vs. handshaking
// TODO: write about semantic choices late ron in report
// TODO: syntactic classes separate here for variables and names
case class SendInt(c: String, v: Int) extends Instruction
case class SendValue(c: String, n: String) extends Instruction

// TODO: different names? possible to be clearer
// TODO: better types
// TODO: let x in ... ? similar bind
// TODO: possibly include a value for new int
case class NewInt(n: String) extends Instruction
case class NewChannel(n: String) extends Instruction
case class Delete(n: String) extends Instruction

case class Read(n: String) extends Instruction
case class Print() extends Instruction