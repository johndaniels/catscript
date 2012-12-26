package catscript.interpret

import scala.collection.mutable.HashMap
import catscript.ast.Statement
import catscript.ast.AssignmentStatement
import catscript.ast.Expression
import catscript.ast.Identifier
import catscript.ast.NumberLiteral
import catscript.ast.BinaryExpression
import catscript.ast.Invocation
import catscript.ast.Block
import catscript.ast.Operator
import catscript.ast.Func
import catscript.ast.ExpressionStatement

class Interpretor {
  private trait CatObject {
    
  }
  private implicit def toCatDouble(old: Double) = new CatDouble(old)
  private case class CatDouble(value: Double) extends CatObject {}
  private case class CatFunc(function: Func) extends CatObject {
    def call(arguments: List[Expression]):CatObject = {
      function.expression match {
        case expression => evaluate(expression)
      }
    }
  }
  private case class CatNone() extends CatObject {}
  
  private val variables = new HashMap[String,CatObject]
  
  
  private def evaluate(expr:Expression): CatObject = {
    expr match {
      case Identifier(name) => {
        variables.get(name).get
      }
      case NumberLiteral(number) => {
        number
      }
      case BinaryExpression(leftExpr, Operator(operator), rightExpr) => {
        var left = evaluate(leftExpr)
        var right = evaluate(rightExpr)
        (left, right) match {
          case (CatDouble(leftValue), CatDouble(rightValue)) => {
            operator match {
	          case "-" => leftValue - rightValue
	          case "+" => leftValue + rightValue
	          case "/" => leftValue / rightValue
	          case "*" => leftValue * rightValue
            }
          }
        }
      }
      case function: Func => {
        CatFunc(function)
      }
      case Invocation(Identifier(name), arguments) => {
        val maybeValue = variables.get(name)
        maybeValue match {
          case Some(function: CatFunc) => {
            function.call(arguments)
          }
        }
      }
      case Block(statements) => {
        var lastValue: CatObject = CatNone()
        for (statement <- statements) {
          lastValue = statement match {
            case ExpressionStatement(expression) => evaluate(expression)
          }
        }
        return lastValue
      }
    }
  }
  
  def run(statement:Statement): Unit = {
    statement match {
      case AssignmentStatement(identifier, expression) => {
        variables.put(identifier.name, evaluate(expression))
      }
      case ExpressionStatement(invocation) => {
        println(evaluate(invocation))
      }
    }
  }
}