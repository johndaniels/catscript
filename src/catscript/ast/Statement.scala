package catscript.ast

trait Statement extends Token {

}

case class AssignmentStatement(identifier: Identifier, expression: Expression) extends Statement { }
case class ExpressionStatement(expression: Expression) extends Statement { }
