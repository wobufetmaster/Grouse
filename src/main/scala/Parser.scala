
import cats.parse.{Parser => P, Parser0, Numbers}
import cats.parse.Rfc5234.{sp, alpha, digit}
import scala.math.BigDecimal
import scala.util.Try
object Parser:
  sealed trait Expr
  case class Binary(left: Expr, operator: String, right: Expr) extends Expr
  case class Unary(operator: String, right: Expr) extends Expr
  case class Literal(value: BigDecimal) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Identifier(name: String) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr

  case object NilLiteral extends Expr
  case class FunctionLiteral(params: List[Identifier], body: Expr) extends Expr

  /**
   * Parses various literals in a JSON-like language.
   * 
   * - `numberParser` parses a JSON number and maps it to a `Literal` of type `BigDecimal`.
   * - `identifierParser` parses an identifier consisting of alphabetic characters followed by zero or more alphanumeric characters, and maps it to an `Identifier`.
   * - `stringParser` parses a string enclosed in double quotes and maps it to a `StringLiteral`.
   * - `booleanParser` parses the strings "true" or "false" and maps them to a `BooleanLiteral`.
   * - `nilParser` parses the string "nil" and maps it to a `NilLiteral`.
   * - `literalParser` parses any of the above literals and maps them to an `Expr`.
   */
  val number = Numbers.jsonNumber.map(BigDecimal(_)).map(Literal.apply).backtrack
  val identifier: P[Identifier] = (alpha ~ (alpha | digit).rep0).string.map(Identifier.apply)
  val stringLiteral: P[StringLiteral] = (P.char('"') *> P.charsWhile(_ != '"').string <* P.char('"')).map(StringLiteral.apply)
  val booleanLiteral: P[BooleanLiteral] = (P.string("true") | P.string("false")).string.map(_.toBoolean).map(BooleanLiteral.apply)
  val nilLiteral: P[NilLiteral.type] = P.string("nil").as(NilLiteral)

  lazy val expression: P[Expr] = P.defer(binop.backtrack | literal.backtrack | unaryop.backtrack | grouping)

  val term: P[Expr] = P.defer(grouping | unaryop | literal)

  val literal: P[Expr] = number | stringLiteral | booleanLiteral | nilLiteral | identifier

  val binaryOperator = (P.string("+") | P.string("-") | P.string("*") | P.string("/") | P.string("%")).string

  val unaryOperator = (P.string("-") | P.string("!")).string

  val binop: P[Expr] = (term ~ (binaryOperator ~ term).rep).map {
  case (left, rest) => rest.foldLeft(left) { case (acc, (op, right)) => Binary(acc, op, right) }
}

  //val binop: P[Expr] = (term ~ binaryOperator ~ term).map { case ((left: Expr, operator: String), right: Expr) => Binary(left, operator, right) }
  //val binop: P[Expr] = (expression ~ P.string("+").string).map(_ => StringLiteral("Hello")) 

  val unaryop: P[Unary] = (unaryOperator ~ expression).map { case (operator: String, right: Expr) => Unary(operator, right) }

  val grouping: P[Grouping] = (P.char('(') *> expression <* P.char(')')).map(Grouping.apply)

  val whitespaces = sp.rep0.void
  def lexeme[A](p: P[A]): P[A] = p <* whitespaces

  //def keyword(s: String): P[Unit] = (P.string(s) ~ P.peek((P.charIn(" \t\r\n.;,({})") | P.end))).void.backtrack
  

  val whitespace: P[Unit] = P.charsWhile(c => c.isWhitespace).void
  def keyword(s: String): P[Unit] = (P.string(s) ~ P.peek((sp | P.end))).void.backtrack

  def parse(source: String) =
    println(expression.parseAll(source))
    