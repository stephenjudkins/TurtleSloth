import org.specs._

import org.specs.matcher.Matcher

import org.sdj.turtlesloth.Program

case class output(expected: String) extends Matcher[String]() {
  def apply(source: => String) = {
    val program = Program.fromSource(source)
    val output = program.completion.output
    (output == expected, "awesome!",  "%s expected; got %s".format(expected, output))
  }
}

class BasicSpec extends Specification {
  "hello world" in {
    "print(\"hello world\")" must output("hello world\n")
  }

  "hello world in two lines" in {
    "print(\"hello\"); print(\"world\");" must output("hello\nworld\n")
  }

  "method definition and call" in {
    "f = function() { print(\"called\");} f();" must output("called\n")
  }

//  "method definition and call with arguments" in {
//    "f = function(a) { print(a); }; f(\"argument\");" must output("argument\n")
//  }

  "assignment" in {
    "a = \"foo\";\nprint(a)" must output("foo\n")
  }


}
