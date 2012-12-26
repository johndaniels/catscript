package catscript.ast

trait Token {
  
}

case class ParameterList(arguments: List[Identifier])
case class Operator(operator: String) extends Token {}