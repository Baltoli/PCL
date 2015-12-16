package uk.ac.cam.bsc28.diss.FrontEnd

trait ASTNode
trait ExpressionNode extends ASTNode

/*
 * We have two basic kinds of 'names' in the language, either of which could
 * be a node in the AST - variable names and channel names. The two types are
 * distinct, but there are both places where they can be interchanged, and
 * places where they cannot be. To solve this we introduce a new trait for
 * general names (i.e. either a variable name or a channel name).
 */

trait Name extends ExpressionNode
case class VariableName(n: String) extends Name
case class ChannelName(n: String) extends Name


/*
 * We have arithmetic operations that can be performed on integers, and so we
 * define a trait for operation types.
 *
 * Currently the language only defines binary operations on numbers, so we only
 * need a binary operation case.
 */

trait ArithmeticNode extends ExpressionNode
case class IntegerLiteral(v: Long) extends ArithmeticNode

trait Operation
case class AddOp() extends Operation
case class MultiplyOp() extends Operation
case class SubtractOp() extends Operation
case class DivideOp() extends Operation

case class BinaryOperation(op: Operation, left: ArithmeticNode, right: ArithmeticNode) extends ArithmeticNode

/*
 * Processes represent things being 'done' in the language (sending, receiving etc.).
 */

trait ProcessNode extends ASTNode
case class Out(chan: Name, value: ExpressionNode) extends ProcessNode
case class In(chan: Name, variable: VariableName) extends ProcessNode
case class Parallel(left: ProcessNode, right: ProcessNode) extends ProcessNode
case class Replicate(proc: ProcessNode) extends ProcessNode
case class Let(variable: VariableName, value: ExpressionNode, proc: ProcessNode) extends ProcessNode
case class Sequential(first: ProcessNode, second: ProcessNode) extends ProcessNode

/*
 * A condition can only appear in its proper place - it cannot be used as an
 * expression, and so we implement it as a distinct class.
 */
case class Condition(left: ExpressionNode, right: ExpressionNode) extends ASTNode
case class If(cond: Condition, proc: ProcessNode) extends ProcessNode

/*
 * BNF Grammar for the language:
 *
 * Name             ::= var_name
 *                  |   channel_name
 *
 * Op               ::= plus | minus | multiply | divide
 *
 * ArithmeticNode   ::= integer_literal
 *                  |   ArithmeticNode Op ArithmeticNode
 *
 * ExpressionNode   ::= Name
 *                  |   ArithmeticNode
 *
 * ProcessNode      ::= out Name op_br ExpressionNode cl_br
 *                  |   in Name op_br var_name cl_br
 *                  |   op_br ProcessNode par ProcessNode cl_br
 *                  |   rep op_br ProcessNode cl_br
 *                  |   op_sq ExpressionNode eq ExpressionNode cl_sq op_cr ProcessNode cl_cr
 *                  |   let var_name eq ExpressionNode op_cr ProcessNode cl_cr
 *                  |   ProcessNode seq ProcessNode
 *                  |   end
 *
 * TODO: eliminate left recursion from grammar
 * TODO: codify this grammar in its current form in notes
 * TODO: we can probably simplify the type hierarchy to just ASTNodes with named cases
 * TODO: check that recursive descent parsing will actually work for this grammar by generating first, follow, nullable
 */
