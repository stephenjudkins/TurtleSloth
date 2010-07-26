package org.sdj.turtlesloth
import org.sdj.turtlesloth.ast._
import org.sdj.turtlesloth.builtins._

class Program(block: Block) {
  lazy val initialState = new State(this, firstStack)
  val states = Stream.iterate(initialState)(_.nextState)

  lazy val completion = states.filter(_.isTerminated).head

  lazy val firstStack = Stack(Frame(List(), block.contents.toList, DefaultGlobals.get))
}

case class Stack(frames: Frame*) {
  lazy val currentFrame = frames.head
  lazy val isTerminated = frames.length == 1 && currentFrame.isTerminated
  lazy val currentStatement = currentFrame.current

  def withUpdatedFrame(newFrame: Frame) = Stack((newFrame :: frames.tail.toList): _*)
}

case class Frame(previous: List[Statement], next: List[Statement], variables: Map[String, Any] = Map()) {
  lazy val current = next.head
  lazy val isTerminated = next.isEmpty

  def get(name: String) = variables.get(name)
}

case class State(
  program: Program,
  stack: Stack,
  outputStrings:List[String] = List(),
  sequenceNo: Int = 0) {

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

  lazy val updatedState = {
    val updated = currentStatement.update(this)

    updated.copy(
      stack = updated.toNextStatement,
      sequenceNo = sequenceNo + 1
    )
  }

  lazy val toNextStatement =
    stack.withUpdatedFrame(
      currentFrame.copy(
        previous = currentFrame.previous ::: currentFrame.current :: Nil,
        next = currentFrame.next.tail
      )
    )

  lazy val output = outputStrings.reverse.mkString

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

