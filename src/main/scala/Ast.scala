package org.sdj.turtlesloth.ast
import org.sdj.turtlesloth._

class Node

trait Noop {
  def update(state: State):State = state
}

class Expression extends Node with Noop {
  def resolve(state: State):(Option[Any], State) = (None, state)
}

class Statement extends Node with Noop {
}


case class Return(value: Expression) extends Statement
case class Block(contents: Statement*) extends Statement

case class ExpressionStatement(expression: Expression) extends Statement {
  override def update(state: State) = expression.update(state)
}



case class ArrayExpression(a: Expression*) extends Expression

case class Assignment(to: Expression, value: Expression) extends Expression {

  override def update(state: State) = {
    to match {
      case Identifier(name) => assignToVariable(name, state).toNextStatement
      case _ => throw new Exception("i don't know what to do here")
    }
  }

  def assignToVariable(name: String, state: State) = {

    val (resolvedValue, updatedState) = value.resolve(state)

    updatedState.withCurrentFrameUpdated {(currentFrame) =>
      currentFrame.copy(variables = currentFrame.variables.updated(name, resolvedValue.get))
    }
  }
}

case class Identifier(name: String) extends Expression {
  override def resolve(state: State) = (state.currentFrame.get(name), state)
}

case class MethodCall(method: Expression, args:List[Expression]) extends Expression {
  override def update(state: State) = {
    val (methodValue, state2) = method.resolve(state)

    val (vals, state3) = resolveArgs(state2)

    methodValue match {
      case Some(m: Method) => updateWithMethodAndArgs(m, vals, state3)
      case _ => throw new Exception("not a method!")
    }
  }

  def updateWithMethodAndArgs(method:Method, args:List[Any], state: State) = {
    val (result, updated) = method.resolve(args, state)

    updated
  }

  def resolveArgs(state: State) = {
    args.foldLeft((List[Any](), state)) { case ((l, s), exp) =>
      val (value, updated) = exp.resolve(s)

      (l ::: value.get :: Nil, updated)
    }
  }
}


class Literal extends Expression

case class StringLiteral(content: String) extends Literal {
  override def resolve(state: State) = (Some(content), state)
}

case class NumericLiteral(value:Long) extends Literal
case class BooleanLiteral(value: Boolean) extends Literal

case object ThisExpression extends Expression

case class MethodExpression(params: ParameterList, block: Block) extends Expression {
  override def resolve(state: State) = (Some(method), state)

  lazy val method = new Method {
    override def resolve(args: List[Any], state: State) = {
      val shifted = state.withNextStatement

      val updated = shifted.copy(
        stack = Stack(blockFrame(shifted))
      )

      (None, updated)
    }

  }

  def blockFrame(state: State) = {
    state.currentFrame.copy(previous = List(), next = block.contents.toList, parent = Some(state.currentFrame))
  }

  lazy val noop = new Statement with Noop

}

case class ParameterList(params: String*)