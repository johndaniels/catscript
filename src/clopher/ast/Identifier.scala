package clopher.ast

import clopher.ast.Token

class Identifier(text: String) extends Expression {
  val name = text;
}