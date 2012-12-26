import scala.util.parsing.combinator.RegexParsers
import catscript.ast.Identifier
import catscript.ast.Operator
import catscript.ast.Expression
import catscript.ast.BinaryExpression
import catscript.ast.IdentifierExpression
import catscript.ast.Assignment
import catscript.ast.AssignmentStatement
import catscript.ast.ExpressionStatement
import catscript.ast.NumberLiteral
import scala.util.parsing.combinator.PackratParsers
import catscript.ast.ParameterList
import catscript.ast.Statement
import catscript.ast.Func
import catscript.ast.Block
import catscript.ast.Invocation

object Parser extends RegexParsers with PackratParsers {
  val ID = """[a-zA-Z]([a-zA-Z0-9]|_)*""".r ^^ { new Identifier(_)}
  val PLUS = "+" ^^ { new Operator(_) }
  val MINUS = "-" ^^ { new Operator(_) }
  val DIVIDE = "/" ^^ { new Operator(_) }
  val MULTIPLY = "*" ^^ { new Operator(_) }
  val ASSIGN = "=" ^^ ((_) => new Assignment)
  val NUMBER = """([0-9]*\.[0-9]+)|([0-9]+\.?)""".r ^^ { new NumberLiteral(_);}
  val SEMICOLON = ";"
  val COMMA = ","
  
  val LPAREN = "("
  val RPAREN = ")"
  val LCURLY = "{"
  val RCURLY = "}"
  val FAT_ARROW = "=>"
 
  val ARGUMENTS = (EXPRESSION ~ ((COMMA ~> EXPRESSION)*)).? ^^ {
    _ match {
      case Some(~(expr:Expression, exprList:List[Expression])) => {
        expr :: exprList
      }
      case None => {
        List()
      }
    }
  }
    
  val INVOCATION = (ID ~ (LPAREN ~> ARGUMENTS <~ RPAREN)) ^^ {
    _ match {
      case ~(id:Identifier, arguments:List[Expression]) => {
        Invocation(id, arguments)
      }
    }
  }
  
  lazy val TERM:PackratParser[Expression] = (FUNCTION | INVOCATION | ID | NUMBER | ("(" ~> EXPRESSION <~ ")"))
  
  private def getBinopParser(binop:Parser[Operator], expr:Parser[Expression], desc:Parser[Expression]) = {
    ((expr ~ binop ~ desc) | desc) 
  } ^^ {
    _ match {
      case ~(~(a:Expression,b:Operator),c:Expression) => {
        new BinaryExpression(a, b, c)
      }
      case b:Expression => {
        b
      }
    }
  }
  
  lazy val PROD:PackratParser[Expression] = getBinopParser((MULTIPLY|DIVIDE), PROD, TERM)
  
  lazy val SUM:PackratParser[Expression] = getBinopParser((PLUS|MINUS), SUM, PROD)
  
  val PARAMS_LIST:PackratParser[ParameterList] = (ID ~ ((COMMA ~> ID)*)) ^^ {
    _ match {
      case ~(id:Identifier, idList:List[Identifier]) => {
        ParameterList(id :: idList)
      }
    }
  }
  
  val PARAMS:PackratParser[ParameterList] = ((LPAREN ~> PARAMS_LIST.? <~ RPAREN) | PARAMS_LIST) ^^ {
    _ match {
      case Some(args:ParameterList) => {
        args
      }
      case None => {
        ParameterList(List())
      }
      case args:ParameterList => {
        args
      }
    }
  }
  
  val BLOCK:PackratParser[Block] = (LCURLY ~> (STATEMENT*) <~ RCURLY) ^^ {
    _ match {
      case list:List[Statement] => {
        Block(list)
      }
    }
  }
  
  lazy val FUNCTION = ((PARAMS <~ FAT_ARROW) ~ (BLOCK | EXPRESSION)) ^^ {
    _ match {
      case ~(arguments:ParameterList, expression:Expression) => {
    	Func(arguments, expression)  
      }
    }
  }
  
  lazy val EXPRESSION:PackratParser[Expression] = SUM
  
  val STATEMENT = BLOCK | (((ID ~ ASSIGN ~ EXPRESSION) <~ SEMICOLON) | (INVOCATION <~ SEMICOLON)) ^^ {
    _ match {
      case ~(~(x:Identifier,y:Assignment), z:Expression) => new AssignmentStatement(x,z)
      case x:Expression => new ExpressionStatement(x)
    }
  }
  
  def apply(s: String) = {
    this.parseAll(STATEMENT, s)
  } 
}