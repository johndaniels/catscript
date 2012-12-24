import scala.util.parsing.combinator.RegexParsers
import clopher.ast.Identifier
import clopher.ast.Operator
import clopher.ast.Expression
import clopher.ast.BinaryExpression
import clopher.ast.IdentifierExpression
import clopher.ast.Assignment
import clopher.ast.AssignmentStatement
import clopher.ast.ExpressionStatement
import clopher.ast.NumberLiteral

object Parser extends RegexParsers {
  val ID = """[a-zA-Z]([a-zA-Z0-9]|_)*""".r ^^ { new Identifier(_)}
  val PLUS = "+" ^^ { new Operator(_) }
  val MINUS = "-" ^^ { new Operator(_) }
  val DIVIDE = "/" ^^ { new Operator(_) }
  val MULTIPLY = "*" ^^ { new Operator(_) }
  val ASSIGN = "=" ^^ ((_) => new Assignment)
  val NUMBER = """([0-9]*\.[0-9]+)|([0-9]+\.?)""".r ^^ { new NumberLiteral(_);}
      
  val TERM = (ID | NUMBER | ("(" ~> EXPR <~ ")"))
  
  private def getBinopParser(binop:Parser[Operator], expr:Parser[Expression]) = {
    (((expr ~ binop)*) ~ expr)
  } ^^ {
    _ match {
      case ~(list, expr:Expression) => {
        if (!list.isEmpty) {
          var result:Expression = list.head._1;
          var lastOperator = list.head._2;
          val remaining = list.drop(1)
        
          for(item <- remaining) {
            result = new BinaryExpression(result, lastOperator, item._1)
            lastOperator = item._2;
          }
          new BinaryExpression(result, lastOperator, expr)
        } else {
          expr
        }
      }
    }
  }
  
  val PROD:Parser[Expression] = getBinopParser((MULTIPLY|DIVIDE), TERM)
  
  val EXPR:Parser[Expression] = getBinopParser((MINUS|PLUS), PROD) 

  val STATEMENT = ((ID ~ ASSIGN ~ EXPR) | EXPR) ^^ {
    _ match {
      case ~(~(x:Identifier,y:Assignment), z:Expression) => new AssignmentStatement(x,z)
      case x:Expression => new ExpressionStatement(x)
    }
  } 
  
  def apply(s: String) = {
    this.parseAll(STATEMENT, s)
  } 
}