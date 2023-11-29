import Lexer.*
import cats.parse.Parser
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
}

class MainSpec extends munit.FunSuite {
  test("Plus parser") {
    assertEquals(Lexer.plusParser.parseAll("+"), Right(PlusToken))
  }


  test("Minus parser") {
    assertEquals(Lexer.minusParser.parseAll("-"), Right(MinusToken))
  }
}


class IdentifierLexerTest extends munit.FunSuite {

  test("identifierParser should parse valid identifier tokens") {
    val input = "abc123"
    val expected = IdentifierToken("abc123")
    val result = identifierParser.parse(input)
    assert(result.successful)
    assert(result.get == expected)
  }

  test("identifierParser should fail to parse invalid identifier tokens") {
    val input = "123abc"
    val result = identifierParser.parse(input)
    assert(result.failure)
  }

}