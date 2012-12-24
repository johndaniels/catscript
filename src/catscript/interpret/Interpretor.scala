package catscript.interpret

import scala.collection.mutable.HashMap
import catscript.ast.Statement
import catscript.ast.AssignmentStatement
import catscript.ast.Expression
import catscript.ast.Identifier
import catscript.ast.NumberLiteral
import catscript.ast.BinaryExpression
import catscript.ast.ExpressionStatement

class Interpretor {
  val variables = new HashMap[String,Double]
  
  def evaluate(expr:Expression): Double = {
    expr match {
      case expr:Identifier => {
        variables.get(expr.name).get
      }
      case expr:NumberLiteral => {
        expr.number
      }
      case expr:BinaryExpression => {
        var left = evaluate(expr.left)
        var right = evaluate(expr.right)
        expr.operator.operator match {
          case "+" => left + right
          case "-" => left - right
          case "/" => left / right
          case "*" => left * right
        }
      }
    }
  }
  
  def run(statement:Statement) = {
    statement match {
      case statement:AssignmentStatement => {
        variables.put(statement.identifier.name, evaluate(statement.expression))
      }
      case statement:ExpressionStatement => {
        println(evaluate(statement.expression))
      }
    }
  }
}