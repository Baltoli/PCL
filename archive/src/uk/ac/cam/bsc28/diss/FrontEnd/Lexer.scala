package uk.ac.cam.bsc28.diss.FrontEnd

/**
  * Some thoughts from the variable / atom update to the interpreter - we don't
  * ever actually need type annotations as type mismatch can be handled just fine
  * at runtime (in the name of making the compiler simpler but less friendly).
  *
  * We also have new syntax to think about - instead of `new` we are instead using
  * a let Var = @atom in { ... } syntax. Let is the only new token here.
  */

class LexException(m: String) extends Exception {
  override def getMessage = m
}

object Lexer {
  val varName = """^([A-Z][a-zA-Z]*)(.*)""".r
  val channelName = """^@((?:[a-z]|_)+)(.*)""".r
  val intLiteral = """^(-?[0-9]+)(.*)""".r
  val operator = """^(\+|-|\*|\/)(.*)""".r
  val in = """^in(.*)""".r
  val out = """^out(.*)""".r
  val let = """^let(.*)""".r
  val parallel = """^\|(.*)""".r
  val sequential = """^\.(.*)""".r
  val end = """^end(.*)""".r
  val replicate = """^!(.*)""".r
  val openBracket = """^\((.*)""".r
  val closeBracket = """^\)(.*)""".r
  val openSquareBracket = """^\[(.*)""".r
  val closeSquareBracket = """^\](.*)""".r
  val openCurlyBracket = """^\{(.*)""".r
  val closeCurlyBracket = """^\}(.*)""".r
  val equals = """^=(.*)""".r
  val fresh = """^fresh(.*)""".r

  private def eat(p: String): Option[(Token, String)] = {
    p match {
      case intLiteral(value, rest) => Some(IntegerLiteral(value toInt), rest trim)
      case operator(op, rest) => Some(Operator(op), rest trim)
      case in(rest) => Some(In(), rest trim)
      case out(rest) => Some(Out(), rest trim)
      case let(rest) => Some(Let(), rest trim)
      case parallel(rest) => Some(Parallel(), rest trim)
      case sequential(rest) => Some(Sequential(), rest trim)
      case end(rest) => Some(End(), rest trim)
      case replicate(rest) => Some(Replicate(), rest trim)
      case openBracket(rest) => Some(OpenBracket(), rest trim)
      case closeBracket(rest) => Some(CloseBracket(), rest trim)
      case openSquareBracket(rest) => Some(OpenSquareBracket(), rest trim)
      case closeSquareBracket(rest) => Some(CloseSquareBracket(), rest trim)
      case openCurlyBracket(rest) => Some(OpenCurlyBracket(), rest trim)
      case closeCurlyBracket(rest) => Some(CloseCurlyBracket(), rest trim)
      case equals(rest) => Some(Equals(), rest trim)
      case fresh(rest) => Some(Fresh(), rest trim)

      case varName(name, rest) => Some(VarName(name), rest trim)
      case channelName(name, rest) => Some(ChannelName(name), rest trim)
      case _ => None
    }
  }

  private def tokenize(p: String, acc: List[Token]): List[Token] = {
    eat(p) match {
      case Some((token, rest)) => tokenize(rest, token::acc)
      case None => p match {
        case "" => acc reverse
        case s =>
          throw new LexException(s"Lexing Error when trying to lex remaining text (no valid token found): $s")
      }
    }
  }

  def tokenize(p: String): List[Token] = {
    val oneLine = p filter(_ >= ' ')
    tokenize(p = oneLine, acc = List[Token]())
  }
}