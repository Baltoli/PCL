package uk.ac.cam.bsc28.diss.FrontEnd

import uk.ac.cam.bsc28.diss.FrontEnd.Parser.ParseResult

// TODO: clean up code
// TODO: better error propagation
// TODO: tests

class ParseError(e: String) extends Exception {
  val err = e

  override def getMessage = e
}

object Parser {
  type ParseResult[N <: ParseTree.Node] = Either[N, ParseError]
  private def syntaxError(e: String = "Syntax error!") = {
    Right(new ParseError(e))
  }
}

class Parser(lexed: List[Token]) {

  private val tokens = lexed
  private var tokenIndex = 0

  private def currentToken(): Option[Token] = {
    if (!(tokens isDefinedAt tokenIndex)) {
      None
    } else {
      Some(tokens(tokenIndex))
    }
  }

  private def eat(t: Token): Option[ParseError] = {
    currentToken() match {
      case Some(token) =>
        if (token == t) {tokenIndex += 1; None} else Some(new ParseError("Syntax error!"))
      case None =>
        Some(new ParseError("Internal consistency error - no token at index."))
    }
  }

  def parse(): Option[ParseTree.Start] = {
    matchStart() match {
      case Left(s) =>
        if (currentToken().isEmpty) {
          Some(s)
        } else {
          throw new ParseError("Parse Error: tokens remain at end of parse.")
        }
      case Right(err) =>
        throw err
    }
  }

  private def matchStart(): ParseResult[ParseTree.Start] = {
    matchProcess() match {
      case Left(proc) => Left(ParseTree.ProcessStart(proc))
      case Right(err) => Right(err)
    }
  }

  private def matchName(): ParseResult[ParseTree.Name] = {
    currentToken() match {
      case Some(VarName(vn)) =>
        eat(VarName(vn))
        Left(ParseTree.VariableName(vn))
      case Some(ChannelName(cn)) =>
        eat(ChannelName(cn))
        Left(ParseTree.ChannelName(cn))
      case _ => Parser.syntaxError("Syntax Error: Expected Name")
    }
  }

  private def matchAddOperation(): ParseResult[ParseTree.AddOperation] = {
    currentToken() match {
      case Some(Operator("+")) =>
        eat(Operator("+"))
        Left(ParseTree.AddNode())
      case Some(Operator("-")) =>
        eat(Operator("-"))
        Left(ParseTree.SubtractNode())
      case _ =>
        Parser.syntaxError("Syntax error: unrecognized (add precedence) operation.")
    }
  }

  private def matchMultiplyOperation(): ParseResult[ParseTree.MultiplyOperation] = {
    currentToken() match {
      case Some(Operator("*")) =>
        eat(Operator("*"))
        Left(ParseTree.MultiplyNode())
      case Some(Operator("/")) =>
        eat(Operator("/"))
        Left(ParseTree.DivideNode())
      case _ =>
        Parser.syntaxError("Syntax error: unrecognized (multiply precedence) operation.")
    }
  }

  private def matchFactor(): ParseResult[ParseTree.Factor] = {
    currentToken() match {
      case Some(VarName(vn)) =>
        eat(VarName(vn))
        Left(ParseTree.VariableFactor(ParseTree.VariableName(vn)))
      case Some(IntegerLiteral(iv)) =>
        eat(IntegerLiteral(iv))
        Left(ParseTree.LiteralFactor(iv))

      case _ => Parser.syntaxError("Syntax Error: Expected integer or variable name.")
    }
  }

  private def matchTerm(): ParseResult[ParseTree.Term] = {
    currentToken() match {
      case Some(OpenBracket()) =>
        eat(OpenBracket())
        val exprResult = matchExpression()
        eat(CloseBracket())
        exprResult match {
          case Left(expr) =>
            Left(ParseTree.ParenthesisedExpressionTerm(expr))
          case Right(e) => Right(e)
        }

      case Some(_) =>
        val firstResult = matchFactor ()
        val auxResult = matchTermAux ()
        (firstResult, auxResult) match {
          case (Left (first), Left (aux) ) =>
            Left (ParseTree.FactorAuxTerm (first, aux))

          case (Right (e), _) => Right (e)
          case (_, Right (e) ) => Right (e)
        }

      case _ => Parser.syntaxError("Syntax error: EOF when parsing term.")
    }
  }

  private def matchTermAux(): ParseResult[ParseTree.TermAux] = {
    val opResult = matchMultiplyOperation()

    if (opResult.isLeft) {
      val op = opResult.left.get
      val factorResult = matchFactor()
      val auxResult = matchTermAux()
      (factorResult, auxResult) match {
        case (Left(factor), Left(aux)) =>
          Left(ParseTree.OperatorTermAux(op, factor, aux))

        case (Right(e), _) => Right(e)
        case (_, Right(e)) => Right(e)
      }
    } else {
      Left(ParseTree.EmptyTermAux())
    }
  }

  private def matchExpression(): ParseResult[ParseTree.Expression] = {
    currentToken() match {
      case Some(ChannelName(cn)) =>
        eat(ChannelName(cn))
        Left(ParseTree.ChannelExpression(ParseTree.ChannelName(cn)))

      case Some(_) =>
        val termResult = matchTerm()
        val auxResult = matchExpressionAux()
        (termResult, auxResult) match {
          case (Left(term), Left(aux)) =>
            Left(ParseTree.TermAuxExpression(term, aux))

          case (Right(e), _) => Right(e)
          case (_, Right(e)) => Right(e)
        }

      case _ => Parser.syntaxError("Syntax Error: EOF when parsing expression.")
    }
  }

  private def matchExpressionAux(): ParseResult[ParseTree.ExpressionAux] = {
    val opResult = matchAddOperation()

    if (opResult.isLeft) {
      val op = opResult.left.get
      val termResult = matchTerm()
      val auxResult = matchExpressionAux()
      (termResult, auxResult) match {
        case (Left(term), Left(aux)) =>
          Left(ParseTree.OperatorExpressionAux(op, term, aux))

        case (Right(e), _) => Right(e)
        case (_, Right(e)) => Right(e)
      }
    } else {
      Left(ParseTree.EmptyExpressionAux())
    }
  }

  private def matchProcess(): ParseResult[ParseTree.Process] = {
    currentToken() match {
      case Some(Out()) =>
        eat(Out())
        val varResult = matchName()
        eat(OpenBracket())
        val exprResult = matchExpression()
        eat(CloseBracket())
        val moreResult = matchProcessAux()
        (varResult, exprResult, moreResult) match {
          case (Left(varName), Left(expr), Left(more)) => Left(ParseTree.OutProcess(varName, expr, more))

          case (Right(e), _, _) => Right(e)
          case (_, Right(e), _) => Right(e)
          case (_, _, Right(e)) => Right(e)
        }

      case Some(In()) =>
        eat(In())
        val chanResult = matchName()
        eat(OpenBracket())
        val nameResult = currentToken() match {
          case Some(VarName(vn)) => Some(vn)
          case _ => None
        }
        tokenIndex += 1
        eat(CloseBracket())
        val moreResult = matchProcessAux()
        (chanResult, nameResult, moreResult) match {
          case (Left(chan), Some(name), Left(more)) =>
            Left(ParseTree.InProcess(chan, ParseTree.VariableName(name), more))
          case (Right(e), _, _) =>
            Right(e)
          case (_, None, _) =>
            Parser.syntaxError("Syntax Error: Expected Variable Name for 'in' process.")
          case (_, _, Right(e)) =>
            Right(e)
        }

      case Some(OpenBracket()) =>
        eat(OpenBracket())
        val leftResult = matchProcess()
        eat(Parallel())
        val rightResult = matchProcess()
        eat(CloseBracket())
        val moreResult = matchProcessAux()
        (leftResult, rightResult, moreResult) match {
          case (Left(leftProc), Left(rightProc), Left(more)) =>
            Left(ParseTree.ParallelProcess(leftProc, rightProc, more))

          case (Right(e), _, _) => Right(e)
          case (_, Right(e), _) => Right(e)
          case (_, _, Right(e)) => Right(e)
        }

      case Some(Replicate()) =>
        eat(Replicate())
        eat(OpenBracket())
        val procResult = matchProcess()
        eat(CloseBracket())
        val moreResult = matchProcessAux()
        (procResult, moreResult) match {
          case (Left(proc), Left(more)) => Left(ParseTree.ReplicateProcess(proc, more))

          case (Right(e), _) => Right(e)
          case (_, Right(e)) => Right(e)
        }

      case Some(OpenSquareBracket()) =>
        eat(OpenSquareBracket())
        val leftResult = matchExpression()
        eat(Equals())
        val rightResult = matchExpression()
        eat(CloseSquareBracket())
        eat(OpenCurlyBracket())
        val procResult = matchProcess()
        eat(CloseCurlyBracket())
        val moreResult = matchProcessAux()
        (leftResult, rightResult, procResult, moreResult) match {
          case (Left(leftCond), Left(rightCond), Left(proc), Left(more)) =>
            Left(ParseTree.IfProcess(leftCond, rightCond, proc, more))

          case (Right(e), _, _, _) => Right(e)
          case (_, Right(e), _, _) => Right(e)
          case (_, _, Right(e), _) => Right(e)
          case (_, _, _, Right(e)) => Right(e)
        }

      case Some(Let()) =>
        eat(Let())
        val nameResult = currentToken() match {
          case Some(VarName(vn)) =>
            eat(VarName(vn))
            Some(vn)
          case _ => None
        }
        eat(Equals())
        val exprResult = matchExpression()
        eat(OpenCurlyBracket())
        val procResult = matchProcess()
        eat(CloseCurlyBracket())
        val moreResult = matchProcessAux()
        (nameResult, exprResult, procResult, moreResult) match {
          case (Some(name), Left(expr), Left(proc), Left(more)) =>
            Left(ParseTree.LetProcess(ParseTree.VariableName(name), expr, proc, more))
          case (None, _, _, _) =>
            Parser.syntaxError("Syntax error: Bad variable name in let expression.")
          case (_, Right(e), _, _) => Right(e)
          case (_, _, Right(e), _) => Right(e)
          case (_, _, _, Right(e)) => Right(e)
        }

      case Some(Fresh()) =>
        eat(Fresh())
        val nameResult = currentToken() match {
          case Some(VarName(vn)) =>
            eat(VarName(vn))
            Some(vn)
          case _ => None
        }
        eat(OpenCurlyBracket())
        val procResult = matchProcess()
        eat(CloseCurlyBracket())
        val moreResult = matchProcessAux()
        (nameResult, procResult, moreResult) match {
          case (Some(name), Left(proc), Left(more)) =>
            Left(ParseTree.FreshProcess(ParseTree.VariableName(name), proc, more))

          case (None, _, _) =>
            Parser.syntaxError("Syntax error: Bad variable name in fresh binding.")
          case (_, Right(e), _) => Right(e)
          case (_, _, Right(e)) => Right(e)
        }

      case Some(End()) =>
        eat(End())
        Left(ParseTree.EndProcess())

      case _ =>
        Parser.syntaxError("Syntax Error: Unexpected token when parsing process")
    }
  }

  private def matchProcessAux(): ParseResult[ParseTree.ProcessAux] = {
    currentToken() match {
      case Some(Sequential()) =>
        eat(Sequential())
        val procResult = matchProcess()
        val moreResult = matchProcessAux()
        (procResult, moreResult) match {
          case (Left(proc), Left(more)) =>
            Left(ParseTree.SequentialProcessAux(proc, more))
          case (Right(e), _) => Right(e)
          case (_, Right(e)) => Right(e)
        }

      case _ => Left(ParseTree.EmptyProcessAux())
    }
  }

}