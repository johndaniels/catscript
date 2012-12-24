package clopher.ast

class ExpressionStatement(expr:Expression) extends Statement {
  val expression = expr
}