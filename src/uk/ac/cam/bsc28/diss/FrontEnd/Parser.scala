package uk.ac.cam.bsc28.diss.FrontEnd

import uk.ac.cam.bsc28.diss.FrontEnd.ParseTree.EmptyProcessAux
import uk.ac.cam.bsc28.diss.FrontEnd.Parser.ParseResult

// TODO: clean up code
// TODO: better error propagation
// TODO: tests

class ParseError(e: String) {
  val err = e
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

  def parse() = matchStart()

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
      case _ => Parser.syntaxError("Name")
    }
  }

  private def matchArithmetic(): ParseResult[ParseTree.Arithmetic] = {
    currentToken() match {
      case Some(IntegerLiteral(v)) =>
        eat(IntegerLiteral(v))
        val moreResult = matchArithmeticAux()
        moreResult match {
          case Left(more) => Left(ParseTree.IntegerWithAux(v, more))
          case _ => Parser.syntaxError("Arithmetic: bad aux")
        }

      case _ => Parser.syntaxError("Arithmetic")
    }
  }

  private def matchArithmeticAux(): ParseResult[ParseTree.ArithmeticAux] = {
    Left(ParseTree.EmptyArithmeticAux()) // TODO
  }

  private def matchExpression(): ParseResult[ParseTree.Expression] = {
    currentToken() match {
      case Some(VarName(_)) | Some(ChannelName(_)) =>
        val nameResult = matchName()
        nameResult match {
          case Left(n) => Left(ParseTree.NameExpression(n))
          case _ => Parser.syntaxError("Expression: bad name")
        }

      case Some(IntegerLiteral(_)) =>
        val arithmeticResult = matchArithmetic()
        arithmeticResult match {
          case Left(v) => Left(ParseTree.ArithmeticExpression(v))
          case _ => Parser.syntaxError("Expresssion: bad arithmetic")
        }

      case _ => Parser.syntaxError("Expression")
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
          case _ => Parser.syntaxError("Process: out")
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
            Parser.syntaxError("Syntax Error: Variable name for 'in' process.")
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
          case _ =>
            Parser.syntaxError("Process: parallel")
        }

      case Some(Replicate()) =>
        eat(Replicate())
        eat(OpenBracket())
        val procResult = matchProcess()
        eat(CloseBracket())
        val moreResult = matchProcessAux()
        (procResult, moreResult) match {
          case (Left(proc), Left(more)) => Left(ParseTree.ReplicateProcess(proc, more))
          case _ => Parser.syntaxError("Process: replicate")
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
          case _ => Parser.syntaxError("Process: if")
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

      case Some(End()) =>
        eat(End())
        Left(ParseTree.EndProcess())

      case _ =>
        println(currentToken())
        Parser.syntaxError("Process")
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

      case _ => Left(EmptyProcessAux())
    }
  }

}