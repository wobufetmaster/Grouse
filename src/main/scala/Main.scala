import scala.io.Source
import scala.io.Source._
import scala.io.StdIn.readLine
import scala.util.{Try, Using, Success, Failure}
import scala.annotation.tailrec


import cats.parse.{Parser => P, Parser0, Numbers}
import cats.parse.Rfc5234.{sp, alpha, digit}

object Lexer:
  sealed trait Token

  case class NumberToken(value: Int) extends Token
  case class IdentifierToken(name: String) extends Token
  case class StringToken(value: String) extends Token
  case object PlusToken extends Token
  case object MinusToken extends Token
  case object StarToken extends Token
  case object SlashToken extends Token
  case object LeftParenToken extends Token
  case object RightParenToken extends Token
  case object EqualsToken extends Token
  case object BangToken extends Token
  case object BangEqualsToken extends Token
  case object EqualsEqualsToken extends Token
  case object GreaterToken extends Token
  case object GreaterEqualsToken extends Token
  case object LessToken extends Token
  case object LessEqualsToken extends Token
  case object AndToken extends Token
  case object OrToken extends Token
  case object TrueToken extends Token
  case object FalseToken extends Token
  case object NilToken extends Token
  case object ValToken extends Token
  case object PrintToken extends Token
  case object IfToken extends Token
  case object ElseToken extends Token
  case object WhileToken extends Token
  case object ForToken extends Token
  case object PipeToken extends Token
  case object ComposeToken extends Token
  case object ConsToken extends Token
  case object WalrusToken extends Token
  case object LeftArrowToken extends Token
  case object RightArrowToken extends Token
  case object QuoteToken extends Token
  case object LeftBraceToken extends Token
  case object RightBraceToken extends Token
  case object RangeToken extends Token
  case object EOFToken extends Token

  val intParser: P[Token] = Numbers.signedIntString.map(s => NumberToken(s.toInt))
  val identifierParser: P[Token] = (alpha ~ (alpha | digit).rep0).string.map(IdentifierToken.apply)
  val stringParser: P[Token] = (P.char('"') *> P.charsWhile(_ != '"').string <* P.char('"')).map(StringToken.apply)

  val whitespaces: Parser0[Unit] = P.charIn(" \t\r\n").void.rep0.void
  def lexeme[A](p: P[A]): P[A] = p <* whitespaces
  
  val simple_parsers: List[P[Token]] = List(
    P.string("->").as(RightArrowToken),
    P.string("<-").as(LeftArrowToken),
    P.string("::").as(ConsToken),
    P.string(":=").as(WalrusToken),
    P.string("||").as(OrToken),
    P.string("|>").as(ComposeToken),
    P.string("..").as(RangeToken),
    P.char('|').as(PipeToken),
    P.char('+').as(PlusToken),
    P.char('-').as(MinusToken),
    P.char('*').as(StarToken),
    P.char('/').as(SlashToken),
    P.char('(').as(LeftParenToken),
    P.char(')').as(RightParenToken),
    P.string("==").as(EqualsEqualsToken),
    P.char('=').as(EqualsToken),
    P.string("!=").as(BangEqualsToken),
    P.char('!').as(BangToken),
    P.string(">=").as(GreaterEqualsToken),
    P.char('>').as(GreaterToken),
    P.string("<=").as(LessEqualsToken),
    P.char('<').as(LessToken),
    P.string("&&").as(AndToken),
    P.char('{').as(LeftBraceToken),
    P.char('}').as(RightBraceToken),
  )

  def keyword(s: String): P[Unit] = (P.string(s) ~ P.peek((P.charIn(" \t\r\n.;,({})") | P.end))).void.backtrack

  val keyword_parsers: List[P[Token]] = List(
    keyword("for").as(ForToken),
    keyword("true").as(TrueToken),
    keyword("false").as(FalseToken),
    keyword("nil").as(NilToken),
    keyword("val").as(ValToken),
    keyword("print").as(PrintToken),
    keyword("if").as(IfToken),
    keyword("else").as(ElseToken),
    keyword("while").as(WhileToken)
  )

  val tokenParser: P[Token] = P.oneOf(
    (simple_parsers ++ keyword_parsers :+ stringParser :+ intParser :+ identifierParser).map(lexeme) 
    //P.end.as(EOFToken) 
  )

  def tokenize(input: String): Either[P.Error, List[Token]] = tokenParser.rep0.parseAll(input)

object Parser:
  sealed trait Expr
  case class Binary(left: Expr, operator: Lexer.Token, right: Expr) extends Expr
  case class Unary(operator: Lexer.Token, right: Expr) extends Expr
  case class Literal(value: Int) extends Expr
  case class Grouping(expression: Expr) extends Expr


def readFileAsString(filePath: String): Option[String] =
  Try(Using(Source.fromFile(filePath))(_.mkString).get).toOption

@main def main(script: String*): Unit =
  script match
    case Seq() => run_repl()
    case Seq(script_path) => readFileAsString(script_path) match
      case Some(source) => run_file(source)
      case None => println("File not found!")
    case _ => println("Too many arguments!")

def run_repl(): Unit = 
  println("Running REPL")
  while true do
    val line = readLine("Grouse> ")
    if line == null || line == "exit" then 
      println("All done!")
      return None
    execute(line) match
      case Success(_) => ()
      case Failure(e) => println(e.getMessage)

def execute(source: String): Try[String] =
  val tokens = Lexer.tokenize(source)
  tokens match
    case Left(err) => println(err)
    case Right(tokens) => println(tokens)
  Failure(new Exception("Not implemented"))


def run_file(source: String): Try[String] =
  execute(source)
  Success("Success")