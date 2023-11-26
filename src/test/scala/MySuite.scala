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
  test("Scanner should return the source string") {
    val source = "Hello, World!"
    val scanner = new Scanner(source)
    assert(scanner.Source == source)
  }
}