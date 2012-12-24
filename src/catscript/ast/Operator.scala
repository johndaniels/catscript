package catscript.ast

import catscript.ast.Token

class Operator(op: String) extends Token {
  val operator = op;
}