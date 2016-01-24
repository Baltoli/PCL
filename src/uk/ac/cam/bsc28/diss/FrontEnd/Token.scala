package uk.ac.cam.bsc28.diss.FrontEnd

trait Token

case class VarName(s: String) extends Token
case class ChannelName(c: String) extends Token
case class IntegerLiteral(i: Int) extends Token
case class Operator(o: String) extends Token
case class In() extends Token
case class Out() extends Token
case class Let() extends Token
case class Parallel() extends Token
case class Sequential() extends Token
case class End() extends Token
case class Replicate() extends Token
case class OpenBracket() extends Token
case class CloseBracket() extends Token
case class OpenSquareBracket() extends Token
case class CloseSquareBracket() extends Token
case class OpenCurlyBracket() extends Token
case class CloseCurlyBracket() extends Token
case class Equals() extends Token
case class Fresh() extends Token
