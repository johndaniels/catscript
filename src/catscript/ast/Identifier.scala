package catscript.ast

import catscript.ast.Token

class Identifier(text: String) extends Expression {
  val name = text;
}