package org.sdj.turtlesloth
import org.sdj.turtlesloth.ast._
import org.sdj.turtlesloth.builtins._

class Program(block: Block) {
  lazy val initialState:Option[State] = Some(new State(this, firstStack))
  val states = Stream.iterate(initialState)(_.flatMap {(s) => s.nextState}).takeWhile(_.isDefined).flatten

  lazy val completion = states.take(500).last

  lazy val firstStack = Stack(Frame(List(), block.contents.toList, DefaultGlobals.get))
}

case class Stack(frames: Frame*) {
  lazy val currentFrame = frames.head
  lazy val isTerminated = frames.length == 1 && currentFrame.isTerminated
  lazy val currentStatement = currentFrame.current

  def withUpdatedFrame(newFrame: Frame) = Stack((newFrame :: frames.tail.toList): _*)

  lazy val prettyPrint = frames.map(_.prettyPrint).mkString("\n")

  lazy val toNextStatement =
    withUpdatedFrame(
      currentFrame.copy(
        previous = currentFrame.previous ::: currentFrame.current.toList,
        next = currentFrame.next.tail
      )
    )

  lazy val withoutTopFrame = { Stack(frames.tail: _*) }
}

case class Frame(previous: List[Statement], next: List[Statement], variables: Map[String, Any] = Map()) {
  lazy val current = next.headOption
  lazy val isTerminated = next.isEmpty

  def get(name: String) = variables.get(name)

  lazy val prettyPrint = "==Frame\n=Previous:%s\n=Next:%s\n=Vars:%s".format(previous, next, variables)
}

case class State(
  program: Program,
  stack: Stack,
  outputStrings:List[String] = List(),
  sequenceNo: Int = 0) {

  lazy val currentStatement = stack.currentStatement
  lazy val currentFrame = stack.currentFrame
  lazy val isTerminated = stack.isTerminated
  lazy val toNextStatement = copy(stack = stack.toNextStatement)
  def withUpdatedFrame(frame: Frame) = copy(stack = stack.withUpdatedFrame(frame))

  lazy val nextState:Option[State] = {
    currentStatement match {
      case Some(current) => Some(current.update(this))
      case None => stack.frames.length match {
        case 1 => None
        case _ => { Some(copy(stack = stack.withoutTopFrame)) }
      }
    }
  }

  lazy val output = outputStrings.reverse.mkString

  lazy val withNextStatement = copy(stack = stack.toNextStatement)

  lazy val prettyPrint = "\n====State:%d\n===Output\n\"%s\"\n===Stack\n%s".format(sequenceNo, output, stack.prettyPrint)

  def withCurrentFrameUpdated(updated: (Frame => Frame)) = {
    copy(
      stack = stack.withUpdatedFrame(
        updated(stack.currentFrame)
      )
    )
  }
}

object Program {
  def fromSource(source: String):Program = fromNode(Parser.parse(source))

  def fromNode(node: Node) = fromBlock(node match {
    case b: Block => b
    case s: Statement => Block(s)
    case e: Expression => Block(ExpressionStatement(e))
  })

  def fromBlock(block: Block) = new Program(block)

}

abstract class Method {
  def resolve(args:List[Any], state: State):(Any, State)
}

