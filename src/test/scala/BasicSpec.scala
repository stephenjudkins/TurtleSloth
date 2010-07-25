import org.specs._

import org.specs.matcher.Matcher

/*import org.sdj.turtlesloth.ast._*/
/*import org.sdj.turtlesloth.Parser*/
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

}
