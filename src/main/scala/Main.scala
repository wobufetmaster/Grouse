import scala.io.Source
import scala.io.Source._
import scala.io.StdIn.readLine
import scala.util.{Try, Using, Success, Failure}
import scala.annotation.tailrec


import cats.parse.{Parser => P, Parser0, Numbers}
import cats.parse.Rfc5234.{sp, alpha, digit}

def error(line: Int, message: String): Unit =
 report(line, "", message)
  
def report(line: Int, where: String, message: String): Unit =
  println(s"[line $line] Error $where: $message")

object Lexer:
  sealed trait Token

  case class NumberToken(value: Int) extends Token
  case class IdentifierToken(name: String) extends Token
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
  case object VarToken extends Token
  case object PrintToken extends Token
  case object IfToken extends Token
  case object ElseToken extends Token
  case object WhileToken extends Token
  case object ForToken extends Token
  case object EOFToken extends Token


  
  val intParser: P[Token] = Numbers.signedIntString.map(s => NumberToken(s.toInt))
  val identifierParser: P[Token] = (alpha ~ (alpha | digit).rep0).string.map(IdentifierToken.apply)

  val whitespaces: Parser0[Unit] = P.charIn(" \t\r\n").void.rep0.void

  def lexeme[A](p: P[A]): P[A] = p <* whitespaces
  
  val keywords: List[String] = List("and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "this", "true", "var", "while")
  keywords.map(P.string(_) ~ P.charIn(" \t\r\n(").void)

  //def keyword_parser(keyword: String): P[Token] = P.string(keyword) <* P.charIn(" \t\r\n(").void

  val simple_parsers: List[P[Token]] = List(
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
    P.string("||").as(OrToken)
  )
//<* P.charIn(" \t\r\n(")).backtrack
  def keyword(s: String): P[Unit] = {
  (P.string(s) ~ (P.charIn(" \t\r\n.;,") | P.end)).void.backtrack
}
  val keyword_parsers: List[P[Token]] = List(
    keyword("for").as(ForToken),
    keyword("true").as(TrueToken),
    keyword("false").as(FalseToken),
    keyword("nil").as(NilToken),
    keyword("var").as(VarToken),
    keyword("print").as(PrintToken),
    keyword("if").as(IfToken),
    keyword("else").as(ElseToken),
    keyword("while").as(WhileToken)
  )
  def keyword_helper[A](p: P[A]): P[A] = p <* P.charIn(" \t\r\n(").void.backtrack
  


  val tokenParser: P[Token] = P.oneOf(
    (simple_parsers ++ keyword_parsers :+ intParser :+ identifierParser).map(lexeme)
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
  val result = Lexer.tokenize("42 + x")
  println(result)
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