package clopher.interpret

import scala.collection.mutable.HashMap
import clopher.ast.Statement
import clopher.ast.AssignmentStatement
import clopher.ast.Expression
import clopher.ast.Identifier
import clopher.ast.NumberLiteral
import clopher.ast.BinaryExpression
import clopher.ast.ExpressionStatement

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