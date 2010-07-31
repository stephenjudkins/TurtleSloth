package org.sdj.turtlesloth

import scala.collection.Map
import scala.Int
import scala.util.parsing.combinator._
import org.sdj.turtlesloth.ast._

class ParseException(reason: String) extends Exception(reason)

private class Parser extends RegexParsers {
  // PrimaryExpression	::=	"this"
  // |	ObjectLiteral
  // |	( "(" Expression ")" )
  // |	Identifier
  // |	ArrayLiteral
  // |	Literal

  lazy val primaryExpression: Parser[Expression] =
    thisExpr |
    // objectLiteral |
    "(" ~> expression <~ ")" |
    array |
    literal |
    identifier


  lazy val thisExpr: Parser[Expression] = "this" ^^ { (s) => ThisExpression }

  // Literal	::=	( <DECIMAL_LITERAL> | <HEX_INTEGER_LITERAL> | <STRING_LITERAL> | <BOOLEAN_LITERAL> | <NULL_LITERAL> | <REGULAR_EXPRESSION_LITERAL> )
  lazy val literal: Parser[Literal] = numericLiteral | stringLiteral | booleanLiteral
  lazy val booleanLiteral: Parser[BooleanLiteral] = trueLiteral | falseLiteral
  lazy val trueLiteral: Parser[BooleanLiteral] = "true" ^^ { (s) => BooleanLiteral(true) }
  lazy val falseLiteral: Parser[BooleanLiteral] = "false" ^^ { (s) => BooleanLiteral(false) }
  lazy val numericLiteral: Parser[NumericLiteral] = "\\d+".r ^^ { (n) => NumericLiteral(n.toLong) }
  lazy val stringLiteral: Parser[StringLiteral] =
    "\"" ~> """([^\"[\x00-\x1F]\\]+|\\[\\/bfnrt"]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^
      { (s) => StringLiteral(s.replace("""\/""", "/")) }

  // Identifier	::=	<IDENTIFIER_NAME>
  lazy val identifier: Parser[Identifier] = "[A-Za-z]+".r ^^ { (name) => Identifier(name) }

  // Expression	::=	AssignmentExpression ( "," AssignmentExpression )*
  lazy val expression: Parser[Expression] = assignmentExpression


  // AssignmentExpression	::=	( LeftHandSideExpression AssignmentOperator AssignmentExpression | ConditionalExpression )
  lazy val assignmentExpression: Parser[Expression] =
    realAssignment |
    conditionalExpression

  lazy val realAssignment: Parser[Assignment] =
    (leftHandSideExpression ~ assignmentOperator ~ assignmentExpression) ^^ {
      case left ~ _ ~ expr => Assignment(left, expr)
    }

  // AssignmentOperator	::=	( "=" | "*=" | <SLASHASSIGN> | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" )
  lazy val assignmentOperator: Parser[String] = "\\s*=\\s*".r

  // ConditionalExpression	::=	LogicalORExpression ( "?" AssignmentExpression ":" AssignmentExpression )?
  lazy val conditionalExpression: Parser[Expression] = unaryExpression

  // UnaryExpression	::=	( PostfixExpression | ( UnaryOperator UnaryExpression )+ )
  lazy val unaryExpression: Parser[Expression] = leftHandSideExpression

  lazy val leftHandSideExpression: Parser[Expression] = callExpression | memberExpression

  lazy val callExpression: Parser[MethodCall] = memberExpression ~ arguments ^^ {
    case member ~ args => MethodCall(member, args)
  }

  // MemberExpression	::=	( ( FunctionExpression | PrimaryExpression ) ( MemberExpressionPart )* )
  // |	AllocationExpression
  lazy val memberExpression: Parser[Expression] = methodExpression | primaryExpression

  // FunctionExpression	::=	"function" ( Identifier )? ( "(" ( FormalParameterList )? ")" ) FunctionBody
  lazy val methodExpression: Parser[MethodExpression] = ("function(" ~> parameterList <~ ")") ~ functionBody ^^ {
    case params ~ body => MethodExpression(params, body)
  }

  // FormalParameterList	::=	Identifier ( "," Identifier )*
  lazy val parameterList: Parser[ParameterList] = repsep(identifier, ",") ^^ {(params) =>
    ParameterList(params.map(_.name): _*)
  }

  // FunctionBody	::=	"{" ( SourceElements )? "}"
  lazy val functionBody: Parser[Block] = "{" ~> rep(sourceElement) <~ "}" ^^ { (elements) =>
    Block(elements: _*)
  }


  lazy val arguments: Parser[List[Expression]] = "(" ~> argumentList <~ ")"

  lazy val argumentList: Parser[List[Expression]] = repsep(assignmentExpression, ",")

  // ArrayLiteral	::=	"[" ( ( Elision )? "]" | ElementList Elision "]" | ( ElementList )? "]" )
  lazy val array: Parser[ArrayExpression] = "[" ~> elementList <~"]"
  // ElementList	::=	( Elision )? AssignmentExpression ( Elision AssignmentExpression )*
  lazy val elementList: Parser[ArrayExpression] = repsep(assignmentExpression, ",") ^^ {(list) =>
    ArrayExpression(list: _*)
  }

  // Statement	::=	Block
  // |	JScriptVarStatement
  // |	VariableStatement
  // |	EmptyStatement
  // |	LabelledStatement
  // |	ExpressionStatement
  // |	IfStatement
  // |	IterationStatement
  // |	ContinueStatement
  // |	BreakStatement
  // |	ImportStatement
  // |	ReturnStatement
  // |	WithStatement
  // |	SwitchStatement
  // |	ThrowStatement
  // |	TryStatement
  lazy val statement =
    // variableStatement |
    returnStatement |
    block |
    expressionStatement

  // VariableStatement	::=	"var" VariableDeclarationList ( ";" )?
  // lazy val variableStatement =

  // ReturnStatement	::=	"return" ( Expression )? ( ";" )?
  lazy val returnStatement: Parser[Return] = "return " ~> expression <~ (";"?) ^^ {(exp) =>
    Return(exp)
  }


  // Block	::=	"{" ( StatementList )? "}"
  lazy val block = "{" ~> statementList <~"}"

  // StatementList	::=	( Statement )+
  lazy val statementList: Parser[Block] = rep(statement) ^^ {(statements) =>
    Block(statements: _*)
  }

  // ExpressionStatement	::=	Expression ( ";" )?
  lazy val expressionStatement: Parser[ExpressionStatement] = expression <~ (";"?) ^^ { (e) =>
    ExpressionStatement(e)
  }

  // Elision	::=	( "," )+
  // ObjectLiteral	::=	"{" ( PropertyNameAndValueList )? "}"
  // PropertyNameAndValueList	::=	PropertyNameAndValue ( "," PropertyNameAndValue | "," )*
  // PropertyNameAndValue	::=	PropertyName ":" AssignmentExpression
  // PropertyName	::=	Identifier
  // |	<STRING_LITERAL>
  // |	<DECIMAL_LITERAL>
  // MemberExpressionForIn	::=	( ( FunctionExpression | PrimaryExpression ) ( MemberExpressionPart )* )
  // AllocationExpression	::=	( "new" MemberExpression ( ( Arguments ( MemberExpressionPart )* )* ) )
  // MemberExpressionPart	::=	( "[" Expression "]" )
  // |	( "." Identifier )
  // CallExpression	::=	MemberExpression Arguments ( CallExpressionPart )*
  // CallExpressionForIn	::=	MemberExpressionForIn Arguments ( CallExpressionPart )*
  // CallExpressionPart	::=	Arguments
  // |	( "[" Expression "]" )
  // |	( "." Identifier )
  // Arguments	::=	"(" ( ArgumentList )? ")"
  // ArgumentList	::=	AssignmentExpression ( "," AssignmentExpression )*
  // LeftHandSideExpression	::=	CallExpression
  // |	MemberExpression
  // LeftHandSideExpressionForIn	::=	CallExpressionForIn
  // |	MemberExpressionForIn
  // PostfixExpression	::=	LeftHandSideExpression ( PostfixOperator )?
  // PostfixOperator	::=	( "++" | "--" )
  // UnaryOperator	::=	( "delete" | "void" | "typeof" | "++" | "--" | "+" | "-" | "~" | "!" )
  // MultiplicativeExpression	::=	UnaryExpression ( MultiplicativeOperator UnaryExpression )*
  // MultiplicativeOperator	::=	( "*" | <SLASH> | "%" )
  // AdditiveExpression	::=	MultiplicativeExpression ( AdditiveOperator MultiplicativeExpression )*
  // AdditiveOperator	::=	( "+" | "-" )
  // ShiftExpression	::=	AdditiveExpression ( ShiftOperator AdditiveExpression )*
  // ShiftOperator	::=	( "<<" | ">>" | ">>>" )
  // RelationalExpression	::=	ShiftExpression ( RelationalOperator ShiftExpression )*
  // RelationalOperator	::=	( "<" | ">" | "<=" | ">=" | "instanceof" | "in" )
  // RelationalExpressionNoIn	::=	ShiftExpression ( RelationalNoInOperator ShiftExpression )*
  // RelationalNoInOperator	::=	( "<" | ">" | "<=" | ">=" | "instanceof" )
  // EqualityExpression	::=	RelationalExpression ( EqualityOperator RelationalExpression )*
  // EqualityExpressionNoIn	::=	RelationalExpressionNoIn ( EqualityOperator RelationalExpressionNoIn )*
  // EqualityOperator	::=	( "==" | "!=" | "===" | "!==" )
  // BitwiseANDExpression	::=	EqualityExpression ( BitwiseANDOperator EqualityExpression )*
  // BitwiseANDExpressionNoIn	::=	EqualityExpressionNoIn ( BitwiseANDOperator EqualityExpressionNoIn )*
  // BitwiseANDOperator	::=	"&"
  // BitwiseXORExpression	::=	BitwiseANDExpression ( BitwiseXOROperator BitwiseANDExpression )*
  // BitwiseXORExpressionNoIn	::=	BitwiseANDExpressionNoIn ( BitwiseXOROperator BitwiseANDExpressionNoIn )*
  // BitwiseXOROperator	::=	"^"
  // BitwiseORExpression	::=	BitwiseXORExpression ( BitwiseOROperator BitwiseXORExpression )*
  // BitwiseORExpressionNoIn	::=	BitwiseXORExpressionNoIn ( BitwiseOROperator BitwiseXORExpressionNoIn )*
  // BitwiseOROperator	::=	"|"
  // LogicalANDExpression	::=	BitwiseORExpression ( LogicalANDOperator BitwiseORExpression )*
  // LogicalANDExpressionNoIn	::=	BitwiseORExpressionNoIn ( LogicalANDOperator BitwiseORExpressionNoIn )*
  // LogicalANDOperator	::=	"&&"
  // LogicalORExpression	::=	LogicalANDExpression ( LogicalOROperator LogicalANDExpression )*
  // LogicalORExpressionNoIn	::=	LogicalANDExpressionNoIn ( LogicalOROperator LogicalANDExpressionNoIn )*
  // LogicalOROperator	::=	"||"
  // ConditionalExpressionNoIn	::=	LogicalORExpressionNoIn ( "?" AssignmentExpression ":" AssignmentExpressionNoIn )?
  // AssignmentExpressionNoIn	::=	( LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn | ConditionalExpressionNoIn )
  // Expression	::=	AssignmentExpression ( "," AssignmentExpression )*
  // ExpressionNoIn	::=	AssignmentExpressionNoIn ( "," AssignmentExpressionNoIn )*
  // Statement	::=	Block
  // |	JScriptVarStatement
  // |	VariableStatement
  // |	EmptyStatement
  // |	LabelledStatement
  // |	ExpressionStatement
  // |	IfStatement
  // |	IterationStatement
  // |	ContinueStatement
  // |	BreakStatement
  // |	ImportStatement
  // |	ReturnStatement
  // |	WithStatement
  // |	SwitchStatement
  // |	ThrowStatement
  // |	TryStatement

  // VariableDeclarationList	::=	VariableDeclaration ( "," VariableDeclaration )*
  // VariableDeclarationListNoIn	::=	VariableDeclarationNoIn ( "," VariableDeclarationNoIn )*
  // VariableDeclaration	::=	Identifier ( Initialiser )?
  // VariableDeclarationNoIn	::=	Identifier ( InitialiserNoIn )?
  // Initialiser	::=	"=" AssignmentExpression
  // InitialiserNoIn	::=	"=" AssignmentExpressionNoIn
  // EmptyStatement	::=	";"
  // IfStatement	::=	"if" "(" Expression ")" Statement ( "else" Statement )?
  // IterationStatement	::=	( "do" Statement "while" "(" Expression ")" ( ";" )? )
  // |	( "while" "(" Expression ")" Statement )
  // |	( "for" "(" ( ExpressionNoIn )? ";" ( Expression )? ";" ( Expression )? ")" Statement )
  // |	( "for" "(" "var" VariableDeclarationList ";" ( Expression )? ";" ( Expression )? ")" Statement )
  // |	( "for" "(" "var" VariableDeclarationNoIn "in" Expression ")" Statement )
  // |	( "for" "(" LeftHandSideExpressionForIn "in" Expression ")" Statement )
  // ContinueStatement	::=	"continue" ( Identifier )? ( ";" )?
  // BreakStatement	::=	"break" ( Identifier )? ( ";" )?

  // WithStatement	::=	"with" "(" Expression ")" Statement
  // SwitchStatement	::=	"switch" "(" Expression ")" CaseBlock
  // CaseBlock	::=	"{" ( CaseClauses )? ( "}" | DefaultClause ( CaseClauses )? "}" )
  // CaseClauses	::=	( CaseClause )+
  // CaseClause	::=	( ( "case" Expression ":" ) ) ( StatementList )?
  // DefaultClause	::=	( ( "default" ":" ) ) ( StatementList )?
  // LabelledStatement	::=	Identifier ":" Statement
  // ThrowStatement	::=	"throw" Expression ( ";" )?
  // TryStatement	::=	"try" Block ( ( Finally | Catch ( Finally )? ) )
  // Catch	::=	"catch" "(" Identifier ")" Block
  // Finally	::=	"finally" Block
  // FunctionDeclaration	::=	"function" Identifier ( "(" ( FormalParameterList )? ")" ) FunctionBody
  // Program	::=	( SourceElements )? <EOF>
  // SourceElements	::=	( SourceElement )+
  // SourceElement	::=	FunctionDeclaration
  // |	Statement
  // ImportStatement	::=	"import" Name ( "." "*" )? ";"
  // Name	::=	<IDENTIFIER_NAME> ( "." <IDENTIFIER_NAME> )*
  // JScriptVarStatement	::=	"var" JScriptVarDeclarationList ( ";" )?
  // JScriptVarDeclarationList	::=	JScriptVarDeclaration ( "," JScriptVarDeclaration )*
  // JScriptVarDeclaration	::=	Identifier ":" <IDENTIFIER_NAME> ( Initialiser )?
  // insertSemiColon	::=	java code

  lazy val sourceElement: Parser[Statement] = statement

  lazy val program: Parser[Block] = rep(sourceElement) ^^ {(l) =>
    Block(l: _*)
  }

  def parse(s: String):Node = {
    parseAll(program, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new ParseException(x.toString)
      case x @ Error(msg, _) => throw new ParseException(x.toString)
    }
  }
}


object Parser {
  def parse(s: String): Node = (new Parser).parse(s) match {
    case Block(ExpressionStatement(e)) => e
    case Block(s: Statement) => s
    case n:Node => n
  }
}