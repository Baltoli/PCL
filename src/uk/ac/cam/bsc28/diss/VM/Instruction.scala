package uk.ac.cam.bsc28.diss.VM

// TODO: name review

abstract class Instruction
abstract class StackOperator extends Instruction

case class Push(v: Long) extends StackOperator
case class Load(s: String) extends Instruction
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

// --- Receiving on a Channel ---
// Here we have two possibilities - either the program has a channel
// directly encoded into it (e.g. `in @stdio(x: Int)`), or the channel to
// receive on is behind a layer of indirection (i.e. in a variable, e.g.
// `in Received(x: Int)`). We therefore will need two instructions - one that
// uses a channel directly, and one that performs a dereference first.
//
// For now we will have this instruction use create-or-update semantics - when
// we receive into a variable, if the variable does not yet exist in the env.
// then we need to create it. If it does already exist, then we simply update its
// value. This means that an `in` operation can introduce a variable name to the
// program's environment. In principle this makes compile-time checking of data
// flow a bit more difficult, but we can just kill the program at runtime if it
// tries to access a variable that hasn't yet been introduced.

/**
  * ReceiveDirect represents the case where the program has a channel name directly
  * encoded into it, and so we don't need to perform an environment lookup in order
  * to block on the channel.
  *
  * @param c The channel on which we are trying to receive.
  * @param n The variable name to create-or-update with the new value that will be
  *          received.
  */
case class ReceiveDirect(c: Channel, n: Variable) extends Instruction

/**
  * ReceiveIndirect must look up a channel in the interpreter's environment before it
  * can block. If the given variable name doesn't exist in the environment, then the
  * executing program will crash (or if it contains an integer).
  *
  * @param vc The variable name which we are attempting a lookup from.
  * @param n The variable name which we will create-or-update with the received value.
  */
case class ReceiveIndirect(vc: Variable, n: Variable) extends Instruction

// TODO: consider sending nondeterminism - erlang model vs. handshaking
// TODO: write about semantic choices later on in report
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