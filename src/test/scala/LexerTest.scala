import Lexer.*
import cats.parse.Parser
import munit.Assertions._
import cats.implicits._
import scala.util.Random
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class LexerTest extends munit.FunSuite {

  test("Parse valid identifier tokens") {
    for (i <- 1 to 100) {
      val random_chars = Random.alphanumeric.take(10).mkString
      val input = s"a$random_chars"
      val expected = Right(List(IdentifierToken(input)))
      val result = tokenize(input)
      assertEquals(result, expected)
    }
  }

  test("Parse valid number tokens") {
    for (i <- 1 to 100) {
      val input = Random.nextInt(1000000).toString
      val expected = Right(List(NumberToken(input.toInt)))
      val result = tokenize(input)
      
      assertEquals(result, expected)
    }
  }

  test("Parse string literals") {
    val input = "val language = \"grouse is a programming language!\" "
    val expected = Right(List(ValToken, IdentifierToken("language"), EqualsToken, StringToken("grouse is a programming language!")))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse operator tokens") {
    val input = "a + 1 + 2 / 3 * 4 - 5"
    val expected = Right(List(IdentifierToken("a"), PlusToken, NumberToken(1), PlusToken, NumberToken(2), SlashToken, NumberToken(3), StarToken, NumberToken(4), MinusToken, NumberToken(5)))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse boolean literals") {
    val input = "true false"
    val expected = Right(List(TrueToken, FalseToken))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse nil literal") {
    val input = "nil"
    val expected = Right(List(NilToken))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse if statement") {
    val input = "if (a == 1) { print(a) } else { print(2) }"
    val expected = Right(List(IfToken, LeftParenToken, IdentifierToken("a"), EqualsEqualsToken, NumberToken(1), RightParenToken, LeftBraceToken, PrintToken, LeftParenToken, IdentifierToken("a"), RightParenToken, RightBraceToken, ElseToken, LeftBraceToken, PrintToken, LeftParenToken, NumberToken(2), RightParenToken, RightBraceToken))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse while statement") {
    val input = "while (a == 1) { print(a) }"
    val expected = Right(List(WhileToken, LeftParenToken, IdentifierToken("a"), EqualsEqualsToken, NumberToken(1), RightParenToken, LeftBraceToken, PrintToken, LeftParenToken, IdentifierToken("a"), RightParenToken, RightBraceToken))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  test("Parse for statement") {
    val input = "for (a <- 1..10) { print(a) }"
    val expected = Right(List(ForToken, LeftParenToken, IdentifierToken("a"), LeftArrowToken, NumberToken(1), RangeToken, NumberToken(10), RightParenToken, LeftBraceToken, PrintToken, LeftParenToken, IdentifierToken("a"), RightParenToken, RightBraceToken))
    val result = tokenize(input)
    
    assertEquals(result, expected)
  }

  
}