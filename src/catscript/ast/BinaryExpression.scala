package catscript.ast

import catscript.ast.Expression
import catscript.ast.Operator

class BinaryExpression(a: Expression, op: Operator, b: Expression)  extends Expression {
  val left = a
  val operator = op
  val right = b
}