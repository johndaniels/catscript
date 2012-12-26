package catscript.ast

import catscript.ast.Token

trait Expression extends Token{

}

case class BinaryExpression(left: Expression, operator: Operator, right: Expression) extends Expression {}
case class Block(statements: List[Statement]) extends Expression {}
case class Func(parameters: ParameterList, expression: Expression) extends Expression {}
case class Invocation(identifier: Identifier, arguments: List[Expression]) extends Expression {}
case class Identifier(name: String) extends Expression {}
case class NumberLiteral(number: Double) extends Expression {
  def this(s: String) = this(s.toDouble)
}