package clopher.ast

import clopher.ast.Expression
import clopher.ast.Operator

class BinaryExpression(a: Expression, op: Operator, b: Expression)  extends Expression {
  val left = a
  val operator = op
  val right = b
}