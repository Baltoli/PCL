package uk.ac.cam.bsc28.diss.VM

import uk.ac.cam.bsc28.diss.VM.ExternLoader.ChannelCallable
import uk.ac.cam.bsc28.diss.VM.Types.Atom

class Interpreter(program: List[Instruction], externs: Map[String, ChannelCallable]) extends Runnable {

  val stack = new ArithmeticStack()

  //  TODO: instruction seq copying
  /**
    * In the future it may be useful to not copy the instruction
    * sequence when we spawn a new interpreter off. Instead the
    * scheduler should maintain the instruction sequence and have
    * all the interpreters refer to it. This works because the
    * sequence is never actually modified.
    */

  /**
    * The interpreter environment maps _variables_ to either atoms
    * or integer values.
    */
  var environment = Map[Variable, Atom]()
  var labels = Map[String, Int]()

  var programCounter = 0

  var blocked: Option[Pair[Channel, Variable]] = None

  def copy(): Interpreter = {
    val interpreter = new Interpreter(program, externs)
    interpreter.environment = environment
    interpreter.labels = labels
    interpreter
  }

  def execute(i: Instruction): Unit = {
    i match {
      case stackOp : StackOperator =>
        stack operate stackOp

      case Load(v) =>
        environment get v match {
          case Some(Right(value)) => stack push value

          case Some(Left(_)) => fatalError(s"(Load): Variable ${v.n} has type channel")
          case None => fatalError(s"(Load): No Variable ${v.n} in environment")
        }

      case StoreInt(v) =>
        environment += (v -> Right(stack.pop))

      case StoreChannel(v, c) =>
        environment += (v -> Left(c))

      case CopyVariable(dest, data) =>
        environment += (dest -> environment(data))

      case Label(s) => () // Do nothing when we see a label - we've already extracted them,
                          // and removing them from the program is a lot of work.

      case Jump(s) =>
        programCounter = labels(s)

      case JumpIfZero(s) =>
        if((stack pop) == 0) programCounter = labels(s)

      case JumpIfNonZero(s) =>
        if((stack pop) != 0) programCounter = labels(s)

      case End() =>
        programCounter = -1

      case Spawn(s) =>
        Scheduler.spawn(this, labels(s))

      case LoadAndCompareAtom(n,m) =>
        val eq = environment get m map { ma =>
            environment get n map { na =>
              ma == na
            }
        } match {
          case Some(Some(bv)) => bv
          case _ => false
        }
        stack push (if (eq) 1 else 0)

      case LoadAndCompareToChannel(n, c) =>
        val eq = environment get n match {
          case Some(Left(Channel(vcn))) =>
            c match {
              case Channel(cn) => cn == vcn
              case _ => false
            }
          case _ => false
        }
        stack push (if (eq) 1 else 0)

      // In this case we don't need to look up anything in the
      // environment, so we can just directly notify the thread
      // manager with the atom being sent.
      case SendChannelDirect(chan, data) =>
        val maybe = externs.get(chan.n)
        if (maybe.isEmpty) {
          Scheduler.notifyAll(chan, Left(data))
        } else {
          val callable = maybe.get
          callable.receive(Left(data.n))
        }

      // In this case we need to look up the environment for the
      // channel on which we are sending the atom. If we find an
      // appropriate entry in the environment then we can notify
      // a thread waiting on that channel.
      //
      // We use the nested match Some(Left(...)) as a shortcut -
      // the only valid case for getting the channel is when we
      // have an entry in the environment (Some) and it is a channel
      // (Left).
      case SendChannelIndirect(channelVar, data) =>
        environment get channelVar match {
          case Some(Left(chan)) =>
            val maybe = externs.get(chan.n)
            if (maybe.isEmpty) {
              Scheduler.notifyAll(chan, Left(data))
            } else {
              val callable = maybe.get
              callable.receive(Left(data.n))
            }

          case Some(Right(_)) =>
            fatalError(s"(Send): Variable ${channelVar.n} has type Int")

          case None =>
            fatalError(s"(Send): No Variable ${channelVar.n} in environment")
        }

      case SendIntDirect(chan) =>
        val maybe = externs.get(chan.n)
        val data = stack.pop
        if (maybe.isEmpty) {
          Scheduler.notifyAll(chan, Right(data))
        } else {
          val callable = maybe.get
          callable.receive(Right(data))
        }

      case SendIntIndirect(channelVar) =>
        environment get channelVar match {
          case Some(Left(chan)) =>
            val maybe = externs.get(chan.n)
            val data = stack.pop
            if (maybe.isEmpty) {
              Scheduler.notifyAll(chan, Right(data))
            } else {
              val callable = maybe.get
              callable.receive(Right(data))
            }

          case Some(Right(_)) =>
            fatalError(s"(Send): Error: Variable ${channelVar.n} has type Int")

          case None =>
            fatalError(s"(Send): No Variable ${channelVar.n} in environment")
        }

      // In this case we know the channel but not the data to be
      // sent - we look up the data and throw an error if no data
      // exists under the given variable name. No need for nested
      // matching here as we aren't breaking the Either down any
      // further.
      case SendVariableDirect(chan, varName) =>
        environment get varName match {
          case Some(atom) =>
            val maybe = externs.get(chan.n)
            if (maybe.isEmpty) {
              Scheduler.notifyAll(chan, atom)
            } else {
              val callable = maybe.get
              callable.receive(atom match {
                case Left(a) => Left(a.n)
                case Right(b) => Right(b)
              })
            }

          case None =>
            fatalError(s"(Send): No Variable ${varName.n} in environment")
        }

      // This case just combines the two cases from above where we need
      // to look into the environment. Same use of nested matching as
      // before for the channel variable.
      case SendVariableIndirect(channelVar, varName) =>
        environment get varName match {
          case Some(atom) =>
            environment get channelVar match {
              case Some(Left(chan)) =>
                val maybe = externs.get(chan.n)
                if (maybe.isEmpty) {
                  Scheduler.notifyAll(chan, atom)
                } else {
                  val callable = maybe.get
                  callable.receive(atom match {
                    case Left(a) => Left(a.n)
                    case Right(b) => Right(b)
                  })
                }
              case Some(Right(_)) =>
                fatalError(s"(Send): Error: Variable ${channelVar.n} has type Int")

              case None =>
                fatalError(s"(Send): No Variable ${channelVar.n} in environment")
            }
          case None =>
            fatalError(s"(Send): No Variable ${varName.n} in environment")
        }

      // In this case we know the channel and the variable name that will
      // form a blocking pair, so we can just update `blocked` and call
      // a synchronized wait.
      case ReceiveDirect(c, n) =>
        val maybe = externs.get(c.n)
        if (maybe.isEmpty) {
          blocked = Some((c,n))
          this synchronized wait
        } else {
          val callable = maybe.get
          val data = callable.send() match {
            case Left(a) => Left(Channel(a))
            case Right(b) => Right(b)
          }
          environment += (n -> data)
        }

      // Similar to above, but we need to also look up the channel stored
      // in the variable before we can block on it. Fatal error if no such
      // variable is in the env OR if it's holding an int.
      case ReceiveIndirect(vc, n) =>
        environment get vc match {
          case Some(Left(chan)) =>
            val maybe = externs.get(chan.n)
            if (maybe.isEmpty) {
              blocked = Some((chan, n))
              this synchronized wait
            } else {
              val callable = maybe.get
              val data = callable.send() match {
                case Left(a) => Left(Channel(a))
                case Right(b) => Right(b)
              }
              environment += (n -> data)
            }

          case Some(Right(_)) =>
            fatalError(s"(Receive): Variable ${vc.n} has type Int")

          case None =>
            fatalError(s"(Receive): No Variable ${vc.n} in environment")
        }

      //       note that this approach will also allow possible extension functionality
      //       - for example, providing a method loading approach allowing for externally
      //       provided atoms that run specified code on send / receive? Might need a bit
      //       of a reorg. of the scheduler etc. but the core principle of allowing
      //       external code to perform send / receives is interesting.
      //
      //       If we went with this approach, then it would possibly be a good idea to
      //       include some kind of per-thread exclusivity (possibly optional?) to deal
      //       with concurrency issues. One way to do this would be to force the registered
      //       code to take an interpreter ID as an extra parameter so that if the code
      //       requires it we can enforce synchronisation. With this in mind, the basic idea
      //       would be to have syntax like `extern @chan` which will cause the interpreter
      //       to redirect in / out statements to `@chan` to external code with a given
      //       interface.
      case Let(vn, a) =>
        environment += (vn -> a)

      case Delete(vn) => environment -= vn

      case ParallelGuard(label, count) =>
        Scheduler.parallelGuard(this, label, count)
        for (i <- 0 until count) {
          execute(program(programCounter + i + 1))
        }
        programCounter += count
        this synchronized wait

      case ThreadDone(label) =>
        Scheduler.threadDone(label)
    }
  }

  def extractLabels(): Unit = {
    program.zipWithIndex foreach { p =>
      p._1 match {
        case Label(s) => labels += (s -> p._2)
        case _ => ()
      }
    }
  }

  def run(): Unit = {
    extractLabels()
    do {
      execute(program(programCounter))
      programCounter += 1
    } while (programCounter > 0)
  }

  def receive(c: Channel, v: Atom): Boolean = {
    this synchronized {
      blocked foreach { p =>
        if (c == p._1) {
          environment += (p._2 -> v)
          blocked = None
          this synchronized notify
          return true
        }
      }
      false
    }
  }

  def fatalError(err: String = "ADD ME"): Any = {
    println(s"Runtime Error $err")
    System.exit(6)
  }

}
