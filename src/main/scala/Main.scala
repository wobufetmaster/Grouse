import scala.io.Source
import scala.io.Source._
import scala.io.StdIn.readLine
import scala.util.{Try, Using, Success, Failure}


enum TokenType:
  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
   COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
   BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

  // Literals.
   IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
       PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, EOF


class Token(token_type: TokenType = TokenType.WHILE, lexeme: String = "", literal: Object = null, line: Int = 0):
  override def toString(): String = 
    s"$token_type $lexeme $literal $line"

class Scanner(val Source: String):
  var tokens: List[Token] = List()

  var start: Int = 0
  var current: Int = 0
  var line: Int = 1

  def isAtEnd(): Boolean = current >= Source.length
  def scan_token(): Unit =
    val c = advance()
    c match
      case '(' => add_token(TokenType.LEFT_PAREN)
      case ')' => add_token(TokenType.RIGHT_PAREN)
      case '{' => add_token(TokenType.LEFT_BRACE)
      case '}' => add_token(TokenType.RIGHT_BRACE)
      case ',' => add_token(TokenType.COMMA)
      case '.' => add_token(TokenType.DOT)
      case '-' => add_token(TokenType.MINUS)
      case '+' => add_token(TokenType.PLUS)
      case ';' => add_token(TokenType.SEMICOLON)
      case '*' => add_token(TokenType.STAR)
      case '!' => add_token(if match_next('=') then TokenType.BANG_EQUAL else TokenType.BANG)
      case '=' => add_token(if match_next('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
      case '<' => add_token(if match_next('=') then TokenType.LESS_EQUAL else TokenType.LESS)
      case '>' => add_token(if match_next('=') then TokenType.GREATER_EQUAL else TokenType.GREATER)
      case '/' => if match_next('/') then
        while peek() != '\n' && !isAtEnd() do advance()
        else add_token(TokenType.SLASH)
      case ' ' | '\r' | '\t' => ()
      case '\n' => line += 1
      case '"' => string()
      case _ => error(line, "Unexpected character.")
  
  def number(): Unit =
    while isdigit(peek()) do advance()
    // Look for a fractional part.
    if peek() == '.' && isdigit(peekNext()) then
      // Consume the "."
      advance()
      while isdigit(peek()) do advance()
    add_token(TokenType.NUMBER, Source.substring(start, current))
      
  def scan_tokens(): List[Token] = 
    while !isAtEnd() do
      // We are at the beginning of the next lexeme.
      start = current
      scan_token()
    tokens = tokens :+ Token(TokenType.EOF, "", null, line)
    tokens

  def match_next(expected: Char): Boolean =
    if isAtEnd() then return false
    if Source.charAt(current) != expected then return false
    current += 1
    true
  
  def string(): Unit = 
    while peek() != '"' && !isAtEnd() do
      if peek() == '\n' then line += 1
      advance()
    // Unterminated string.
    if isAtEnd() then
      error(line, "Unterminated string.")
      return
    // The closing ".
    advance()
    // Trim the surrounding quotes.
    val value = Source.substring(start + 1, current - 1)
    add_token(TokenType.STRING, value)
  
  def isdigit(c: Char): Boolean = c >= '0' && c <= '9'

  def peek(): Char = 
    if isAtEnd() then '\u0000'
    else Source.charAt(current)

  def peekNext(): Char =
    if current + 1 >= Source.length then '\u0000'
    else Source.charAt(current + 1)
  
  def advance(): Char =  
    current += 1
    Source.charAt(current - 1)

  def add_token(token_type: TokenType): Unit =
    add_token(token_type, null)
    
  def add_token(token_type: TokenType, literal: Object): Unit =
    val text = Source.substring(start, current)
    tokens = tokens :+ Token(token_type, text, literal, line)
  
def readFileAsString(filePath: String): Option[String] =
  Try {
    Using(Source.fromFile(filePath))(_.mkString).get
  }.toOption

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
    val line = readLine("> ")
    if line == null || line == "exit" then 
      println("All done!")
      return None
    run(line)
    println(line)
  None

def run(source: String): Option[String] =
  val scanner = Scanner(source)
  val tokens = scanner.scan_tokens();
  tokens.foreach(println)
  None


def run_file(source: String): Try[String] =
  run(source)
  Success("Success")


def error(line: Int, message: String): Unit =
 report(line, "", message)
  
def report(line: Int, where: String, message: String): Unit =
  println(s"[line $line] Error $where: $message")
  None  