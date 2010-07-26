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

  "boolean literal" in {
    "true" must parseTo(BooleanLiteral(true))
  }

  "boolean literal" in {
    "false" must parseTo(BooleanLiteral(false))
  }

  "string literal" in {
    "\"hello world\"" must parseTo(StringLiteral("hello world"))
  }

  "array with single element" in {
    "[foo]" must parseTo(Array(Identifier("foo")))
  }
  "arrays" in {
    "[1,\"foo\", 5]" must parseTo(Array(NumericLiteral(1), StringLiteral("foo"), NumericLiteral(5)))
  }

  "arrays with spaces" in {
    "[1, 2,   3]" must parseTo(Array(NumericLiteral(1), NumericLiteral(2), NumericLiteral(3)))
  }

  "Identifier" in {
    "foo" must parseTo(Identifier("foo"))
  }

  "assignment" in {
    "a = 5" must parseTo(Assignment(Identifier("a"), NumericLiteral(5)))
  }

  "return statement" in {
    "return foo;" must parseTo(Return(Identifier("foo")))
  }

  "method call" in {
    "foo(bar)" must parseTo(MethodCall(Identifier("foo"), List(Identifier("bar"))))
  }

  "method call with multiple arguments" in {
    "foo(spam, \"eggs\")" must parseTo(
      MethodCall(
        Identifier("foo"), List(Identifier("spam"), StringLiteral("eggs"))
      )
    )
  }

  val fooBlock = Block(
    ExpressionStatement(MethodCall(Identifier("foo"), List(NumericLiteral(1)))),
    ExpressionStatement(MethodCall(Identifier("foo"), List(NumericLiteral(2))))
  )

  "multiple statements" in {
    "foo(1); foo(2)" must parseTo(fooBlock)
  }

  "multiple statements" in {
    "foo(1);\n foo(2)" must parseTo(fooBlock)
  }

  "anonymous method expressions" in {
    "function(a) { foo(a); }" must parseTo(
      MethodExpression(
        ParameterList("a"),
        Block(ExpressionStatement(MethodCall(Identifier("foo"), List(Identifier("a")))))
      )
    )
  }

  "assignment" in {
    "a = 5" must parseTo(
      Assignment(Identifier("a"), NumericLiteral(5))
    )
  }
}