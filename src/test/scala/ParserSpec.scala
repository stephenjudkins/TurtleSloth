import org.specs._

import org.specs.matcher.Matcher

import org.sdj.turtlesloth.ast._
import org.sdj.turtlesloth.Parser

case class parseTo(n: Node) extends Matcher[String]() {
  def apply(v: => String) = {
    val result = Parser.parse(v)
    (result == n, "awesome!",  "%s expected; got %s".format(n, result))

  }
}

class ParserSpec extends Specification {
  "numeric literal" in {
    "1138" must parseTo(NumericLiteral(1138))
  }

  "string literal" in {
    "\"hello world\"" must parseTo(StringLiteral("hello world"))
  }

  "arrays" in {
    "[1,\"foo\", 5]" must parseTo(Array(NumericLiteral(1), StringLiteral("foo"), NumericLiteral(5)))
  }

  "variable" in {
    "foo" must parseTo(Variable("foo"))
  }

  "assignment" in {
    "a = 5" must parseTo(Assignment(Variable("a"), NumericLiteral(5)))
  }
}