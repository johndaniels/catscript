package catscript.ast

trait Token {
  
}

case class ParameterList(arguments: List[Identifier])