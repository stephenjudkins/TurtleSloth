package org.sdj.turtlesloth.ast

class Node

class Expression extends Node
class Statement extends Node

case class Assignment(to: Variable, value: Expression) extends Statement

class Literal extends Expression

case class StringLiteral(content: String) extends Literal
case class NumericLiteral(value:Long) extends Literal

case class Array(a: Expression*) extends Expression

case class Variable(name: String) extends Expression