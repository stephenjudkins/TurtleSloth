package org.sdj.turtlesloth

import scala.collection.Map
import scala.Int
import scala.util.parsing.combinator._
import org.sdj.turtlesloth.ast._

class ParseException(reason: String) extends Exception(reason)

private class Parser extends RegexParsers {
/*  val EmptyMap = Map()*/
/*  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (EmptyMap ++ _)*/

/*  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"*/

/*  def member: Parser[(String, Any)] = string ~ ":" ~ value ^^ {*/
/*    case name ~ ":" ~ value => (name, value)*/
/*  }*/


  // **************************
  def literal: Parser[Literal] = numberLiteral | stringLiteral

  def numberLiteral: Parser[NumericLiteral] = "\\d+".r ^^ { (n) => NumericLiteral(n.toLong) }


  def stringLiteral: Parser[StringLiteral] =
    "\"" ~> """([^\"[\x00-\x1F]\\]+|\\[\\/bfnrt"]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^
      { (s) => StringLiteral(s.replace("""\/""", "/")) }


  // **************************
  def expression: Parser[Expression] = literal | array | variable

/*  def statement: Parser[Any] = assignment*/

  def variable: Parser[Variable] = "\\w+".r ^^ { (name) => Variable(name) }
  
  val EQUALS = "\\s*=\\s*".r
  
  def assignment: Parser[Assignment] = variable ~ EQUALS ~ expression ^^ { 
    case (variable ~ EQUALS ~ expression) => Assignment(variable, expression)
  }

  def array: Parser[Array] = "[" ~> repsep(expression, ",") <~"]" ^^ {(n) => Array(n: _*) }

  def anything: Parser[Node] = expression | statement
  
  def statement: Parser[Statement] = assignment

  def parse(s: String) = {
    parseAll(anything, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new ParseException(x.toString)
      case x @ Error(msg, _) => throw new ParseException(x.toString)
    }
  }
}



object Parser {
  def parse(s: String): Any = (new Parser).parse(s)
}