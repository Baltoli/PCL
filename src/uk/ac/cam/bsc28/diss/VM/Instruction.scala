package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.VM.Types.Atom

// TODO: name review

abstract class Instruction
abstract class StackOperator extends Instruction

/**
  * Pushes an integer value onto the stack.
  *
  * @param value The integer to be pushed onto the stack.
  */
case class Push(value: Long) extends StackOperator

/**
  * A Load instruction attempts to look up a variable in the
  * executing environment for an integer value. If such a value
  * can be found, it is pushed onto the arithmetic stack for use
  * in calculations.
  *
  * @param v The variable name to look for in the environment.
  */
case class Load(v: Variable) extends Instruction
case class Add() extends StackOperator
case class Subtract() extends StackOperator
case class Multiply() extends StackOperator
case class Divide() extends StackOperator
case class CompareEqual() extends StackOperator
case class CompareZero() extends StackOperator

// What we want this to do ultimately is compare the underlying
// channel beneath two variables - i.e. env[n] == env[m].
case class CompareNames(n: Variable, m: Variable) extends Instruction // TODO: rename

// could labels be a better type? Not critical but maybe good
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

// Similarly to a receive instruction, we need to think about names vs. variables when
// we perform a send instruction. We have the same situation as receive r.e. the channel
// name we send on - is it direct or indirect? We must also consider the object being sent
// - I think we have two cases here. Either we have an integer or channel literal in the
// program, which we can implement as a single instruction sending an Atom with the correct
// data inside it, or we have a variable name, in which case we perform a dereference. So
// the permutations we need here are:
//  * Atom Direct
//  * Atom Indirect
//  * Variable Direct
//  * Variable Indirect

case class SendAtomDirect(c: Channel, a: Atom) extends Instruction
case class SendAtomIndirect(vc: Variable, a: Atom) extends Instruction
case class SendVariableDirect(c: Channel, v: Variable) extends Instruction
case class SendVariableIndirect(vc: Variable, v: Variable) extends Instruction

// TODO: different names? possible to be clearer
// TODO: better types
// TODO: let x in ... ? similar bind
// TODO: possibly include a value for new int

// These instructions relate to variable scoping. In the 'pure' pi-calculus there is an
// operation that produces a new name with a limited scope - in this variant language I
// will assume that the universe of names is wholly available from the beginning. However
// it might be useful to have a `let X = (@atom|100) in ...` construct where we can
// introduce a variable with a value without having to receive it on a channel. My initial
// approach to this is now a bit outdated - we now probably want the `let` syntax with the
// same curly braces. In addition, we probably only need one variant, as all we are doing is
// binding an Atom to a Variable. We do still need a scope deletion instruction.
case class Let(vn: Variable, a: Atom) extends Instruction
case class Delete(vn: Variable) extends Instruction

case class Read(vn: Variable) extends Instruction
case class Print() extends Instruction