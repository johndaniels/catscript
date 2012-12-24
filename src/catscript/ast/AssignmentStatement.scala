package catscript.ast

class AssignmentStatement(id: Identifier, expr: Expression) extends Statement {
  val identifier = id;
  val expression = expr;
}