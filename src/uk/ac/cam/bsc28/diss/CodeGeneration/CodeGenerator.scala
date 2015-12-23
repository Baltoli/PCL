package uk.ac.cam.bsc28.diss.CodeGeneration

import uk.ac.cam.bsc28.diss.FrontEnd.ParseTree._
import uk.ac.cam.bsc28.diss.VM._

class CodeGenerator(prog: Start) {

  private object JustVar {
    def apply(name: String) =
      TermAuxExpression(FactorAuxTerm(VariableFactor(VariableName(name)), EmptyTermAux()), EmptyExpressionAux())

    def unapply(e: Expression): Option[String] = {
      e match {
        case
          TermAuxExpression(FactorAuxTerm(VariableFactor(VariableName(name)), EmptyTermAux()), EmptyExpressionAux()) =>
          Some(name)

        case _ => None
      }
    }
  }

  def generate(): List[Instruction] = {
    prog match {
      case ProcessStart(proc) => bytecodeForProcess(proc) ++ List(End())
      case _ => List()
    }
  }

  def bytecodeForProcess(p: Process): List[Instruction] = {
    p match {
      case OutProcess(name, expr, aux) =>
        ((name, expr) match {
          case (ChannelName(cn), ChannelExpression(ChannelName(data))) =>
            List(SendChannelDirect(Channel(cn), Channel(data)))

          case (VariableName(vn), ChannelExpression(ChannelName(data))) =>
            List(SendChannelIndirect(Variable(vn), Channel(data)))

          case (ChannelName(cn), JustVar(data)) =>
            List(SendVariableDirect(Channel(cn), Variable(data)))

          case (VariableName(vn), JustVar(data)) =>
            List(SendVariableIndirect(Variable(vn), Variable(data)))

          case (ChannelName(cn), e) =>
            bytecodeForExpression(e) ++ List(SendIntDirect(Channel(cn)))

          case (VariableName(vn), e) =>
            bytecodeForExpression(e) ++ List(SendIntIndirect(Variable(vn)))
        }) ++ bytecodeForProcessAux(aux)

      case InProcess(chanName, varName, aux) =>
        ((chanName, varName) match {
          case (ChannelName(cn), VariableName(in)) =>
            List(ReceiveDirect(Channel(cn), Variable(in)))

          case (VariableName(vn), VariableName(in)) =>
            List(ReceiveIndirect(Variable(vn), Variable(in)))
        }) ++ bytecodeForProcessAux(aux)

      case ParallelProcess(left, right, aux) =>
        val leftLabel = LabelGenerator.nextLabel()
        val rightLabel = LabelGenerator.nextLabel()
        val endLabel = LabelGenerator.nextLabel()

        List(Spawn(leftLabel), Spawn(rightLabel), Jump(endLabel)) ++
        List(Label(leftLabel)) ++ bytecodeForProcess(left) ++ List(End()) ++
        List(Label(rightLabel)) ++ bytecodeForProcess(right) ++ List(End()) ++
        List(Label(endLabel)) ++ bytecodeForProcessAux(aux)

      case ReplicateProcess(proc, aux) =>
        val procLabel = LabelGenerator.nextLabel()
        val endLabel = LabelGenerator.nextLabel()

        List(Spawn(procLabel), Jump(endLabel), Label(procLabel)) ++
        bytecodeForProcess(proc) ++ List(Jump(procLabel), End(), Label(endLabel)) ++
        bytecodeForProcessAux(aux)

      /*
       * Cases to consider here:
       *
       * -  Two channels can be handled by just pushing a
       *    constant 0/1 to the stack.
       * -  Add a new instruction for variable / channel
       *    comparison that loads the variable, checks
       *    equality, then pushes a value.
       * -  For any other cases, if we have a channel expression
       *    then it is an error.
       * -  We have a specialised instruction for two variables.
       * -  Anything else, we just run each expression separately,
       *    then a subtract and compare to zero.
       */
      case IfProcess(left, right, proc, aux) =>
        val endLabel = LabelGenerator.nextLabel()

        ((left, right) match {
          case (ChannelExpression(ChannelName(lc)), ChannelExpression(ChannelName(rc))) =>
            List(if (lc == rc) {
              Push(1)
            } else {
              Push(0)
            }) ++ List(JumpIfZero(endLabel))

          case (ChannelExpression(ChannelName(c)), JustVar(v)) =>
            List(LoadAndCompareToChannel(Variable(v), Channel(c)), JumpIfZero(endLabel))

          case (JustVar(v), ChannelExpression(ChannelName(c))) =>
            List(LoadAndCompareToChannel(Variable(v), Channel(c)), JumpIfZero(endLabel))

          case (JustVar(lv), JustVar(rv)) =>
            List(LoadAndCompareAtom(Variable(lv), Variable(rv)), JumpIfZero(endLabel))

          case (ChannelExpression(_), _) | (_, ChannelExpression(_)) => List() // TODO: error

          case _ =>
            bytecodeForExpression(left) ++ bytecodeForExpression(right) ++
            List(Subtract(), JumpIfNonZero(endLabel))
        }) ++ bytecodeForProcess(proc) ++ List(End(), Label(endLabel)) ++ bytecodeForProcessAux(aux)

      case LetProcess(VariableName(vn), expr, proc, aux) =>
        (expr match {
          case ChannelExpression(ChannelName(cn)) =>
            List(StoreChannel(Variable(vn), Channel(cn)))

          case _ =>
            bytecodeForExpression(expr) ++ List(StoreInt(Variable(vn)))
        }) ++ bytecodeForProcess(proc) ++ bytecodeForProcessAux(aux)

      case EndProcess() => List(End())
    }
  }

  def bytecodeForProcessAux(aux: ProcessAux): List[Instruction] = {
    aux match {
      case SequentialProcessAux(proc, more) =>
        bytecodeForProcess(proc) ++ bytecodeForProcessAux(more)

      case EmptyProcessAux() => List()
    }
  }

  def bytecodeForExpression(e: Expression): List[Instruction] = {
    e match {
      case TermAuxExpression(t, more) =>
        bytecodeForTerm(t) ++ bytecodeForExpressionAux(more)

      case ChannelExpression(c) => List() // TODO: an error
    }
  }

  def bytecodeForExpressionAux(aux: ExpressionAux): List[Instruction] = {
    aux match {
      case OperatorExpressionAux(op, t, more) =>
        val opInstruction = List(op match {
          case AddNode() => Add()
          case SubtractNode() => Subtract()
        })
        bytecodeForTerm(t) ++ opInstruction ++ bytecodeForExpressionAux(more)

      case EmptyExpressionAux() => List()
    }
  }

  def bytecodeForTerm(t: Term): List[Instruction] = {
    t match {
      case FactorAuxTerm(f, more) =>
        bytecodeForFactor(f) ++ bytecodeForTermAux(more)

      case ParenthesisedExpressionTerm(e) =>
        bytecodeForExpression(e)
    }
  }

  def bytecodeForTermAux(aux: TermAux): List[Instruction] = {
    aux match {
      case OperatorTermAux(op, f, more) =>
        val opInstruction = List(op match {
          case MultiplyNode() => Multiply()
          case DivideNode() => Divide()
        })
        bytecodeForFactor(f) ++ opInstruction ++ bytecodeForTermAux(more)

      case EmptyTermAux() => List()
    }
  }

  def bytecodeForFactor(f: Factor): List[Instruction] = {
    f match {
      case VariableFactor(VariableName(vn)) =>
        List(Load(Variable(vn)))

      case LiteralFactor(v) =>
        List(Push(v))

      case _ => List() // TODO: error
    }
  }

}
