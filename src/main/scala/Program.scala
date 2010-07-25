package org.sdj.turtlesloth
import org.sdj.turtlesloth.ast._
import org.sdj.turtlesloth.builtins._

class Program(block: Block) {
  lazy val initialState = new State(this, firstStack, globals = DefaultGlobals.get)
  val states = Stream.iterate(initialState)(_.nextState)

  lazy val completion = states.filter(_.isTerminated).head

  lazy val firstStack = Stack(Frame(List(), block.contents.toList))
}

case class Stack(frames: Frame*) {
  lazy val currentFrame = frames.head
  lazy val isTerminated = frames.length == 1 && currentFrame.isTerminated
  lazy val currentStatement = currentFrame.current
}

case class Frame(previous: List[Statement], next: List[Statement]) {
  lazy val current = next.head
  lazy val isTerminated = next.isEmpty
}

case class State(
  program: Program,
  stack: Stack,
  output:String ="",
  sequenceNo: Int = 0,
  globals: Map[String, Any] = Map()) {

  lazy val currentStatement = stack.currentStatement
  lazy val currentFrame = stack.currentFrame
  lazy val isTerminated = stack.isTerminated

  lazy val nextState = {
    if (isTerminated) {
      this
    } else {
      updatedState
    }
  }

  lazy val updatedState = currentStatement.update(this).
    copy(
      stack = nextStack,
      sequenceNo = sequenceNo + 1
    )

  lazy val nextStack =
    Stack(
      Frame(
        currentFrame.previous ::: currentFrame.current :: Nil,
        currentFrame.next.tail
      )
    )

}

object Program {
  def fromSource(source: String):Program = {

    val block = Parser.parse(source) match {
      case b: Block => b
      case s: Statement => Block(s)
      case e: Expression => Block(ExpressionStatement(e))
    }

    new Program(block)
  }
}

abstract class Method {
  def resolve(args:List[Any], state: State):(Any, State)
}

