package uk.ac.cam.bsc28.diss.FrontEnd

/*
 * BNF Grammar for the language:
 *
 * Start            ::= ProcessNode
 *
 * Name             ::= var_name
 *                  |   channel_name
 *
 * Op               ::= plus | minus | multiply | divide
 *
 * # Left Recursive
 * ArithmeticNode   ::= integer_literal
 *                  |   ArithmeticNode Op ArithmeticNode
 *
 * ExpressionNode   ::= Name
 *                  |   ArithmeticNode
 *
 * # Left Recursive
 * ProcessNode      ::= out Name op_br ExpressionNode cl_br
 *                  |   in Name op_br var_name cl_br
 *                  |   op_br ProcessNode par ProcessNode cl_br
 *                  |   rep op_br ProcessNode cl_br
 *                  |   op_sq ExpressionNode eq ExpressionNode cl_sq op_cr ProcessNode cl_cr
 *                  |   let var_name eq ExpressionNode op_cr ProcessNode cl_cr
 *                  |   ProcessNode seq ProcessNode
 *                  |   end
 *
 * # We now need to eliminate left recursion from ArithmeticNode and ProcessNode
 *
 * ArithmeticNode   ::= integer_literal ArithmeticNode'
 * ArithmeticNode'  ::= Op ArithmeticNode'
 *                  |   ε
 *
 * ProcessNode      ::= out Name op_br ExpressionNode cl_br ProcessNode'
 *                  |   in Name op_br var_name cl_br ProcessNode'
 *                  |   op_br ProcessNode par ProcessNode cl_br
 *                  |   rep op_br ProcessNode cl_br
 *                  |   op_sq ExpressionNode eq ExpressionNode cl_sq op_cr ProcessNode cl_cr
 *                  |   let var_name eq ExpressionNode op_cr ProcessNode cl_cr
 *                  |   end
 *
 * ProcessNode'     ::= seq ProcessNode ProcessNode'
 *                  |   ε
 *
 */
