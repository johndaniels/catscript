package clopher.ast

class NumberLiteral(s: String) extends Expression {
  val number = s.toDouble
}