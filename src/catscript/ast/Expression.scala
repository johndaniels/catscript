package catscript.ast

import catscript.ast.Token

trait Expression extends Token {

}

case class BinaryExpression(left: Expression, operator: Operator, right: Expression) extends Expression {}
case class Func(parameters: ParameterList, expression: Expression) extends Expression {}
case class Block(statements: List[Statement]) extends Expression with Statement {}
case class Invocation(identifier: Identifier, arguments: List[Expression]) extends Expression {}