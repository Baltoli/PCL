package uk.ac.cam.bsc28.diss.FrontEnd

abstract class ASTNode

case class Name(n: String) extends ASTNode
case class TypeName(n: String) extends ASTNode
case class IntLiteral(v: Int) extends ASTNode
case class Term(t: Either[Name, IntLiteral]) extends ASTNode
case class BinaryOp(op: String, left: ASTNode, right: ASTNode) extends ASTNode
case class OutNode(chan: Name, expr: ASTNode) extends ASTNode
case class InNode(chan: Name, typeName: TypeName) extends ASTNode
case class Par(left: ASTNode, right: ASTNode) extends ASTNode
case class ReplicateProc(proc: ASTNode) extends ASTNode
case class Condition(x: ASTNode, y: ASTNode) extends ASTNode
case class If(c: Condition, proc: ASTNode) extends ASTNode
// TODO: let
case class Seq(left: ASTNode, right: ASTNode) extends ASTNode
case class EndNode() extends ASTNode