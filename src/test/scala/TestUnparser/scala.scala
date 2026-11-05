package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite

class TestUnparser extends AnyFunSuite:

  private def parse(source: String): Program =
    val result = StatementParser.parseAll(
      StatementParser.program,
      source
    )
    assert(result.successful, s"Could not parse: $source")
    result.get

  private def unparse(source: String): String =
    Unparser.unparse(parse(source))

  test("unparse a simple assignment"):
    assert(unparse("x = 5;") == "x = 5;")

  test("unparse multiple assignments"):
    assert(unparse("x = 5; y = 7;") == "x = 5;\ny = 7;")

  test("unparse arithmetic expressions"):
    assert(unparse("x = 3 + 4 * 5;") == "x = (3 + (4 * 5));")

  test("unparse subtraction"):
    assert(unparse("x = 8 - 3;") == "x = (8 - 3);")

  test("unparse unary minus"):
    assert(unparse("x = -5;") == "x = (-5);")

  test("unparse an expression statement"):
    assert(unparse("x + 2;") == "(x + 2);")

  test("unparse an if statement"):
    assert(
      unparse("if (x) { y = 2; }") ==
        "if (x) {\n  y = 2;\n}"
    )

  test("unparse an if-else statement"):
    assert(
      unparse("if (x) { y = 2; } else { y = 3; }") ==
        "if (x) {\n  y = 2;\n} else {\n  y = 3;\n}"
    )

  test("unparse a while loop"):
    assert(
      unparse("while (x) { x = x - 1; }") ==
        "while (x) {\n  x = (x - 1);\n}"
    )

  test("unparse a nested block"):
    assert(
      unparse("{ x = 5; }") ==
        "{\n  x = 5;\n}"
    )

  test("parsing and unparsing preserves the AST"):
    val original = parse(
      "x = 3 + 4; if (x) { y = 2; } else { y = 3; }"
    )
    val result = parse(Unparser.unparse(original))
    assert(result == original)

  test("parsing and unparsing preserves a while loop"):
    val original = parse("while (x) { x = x - 1; }")
    val result = parse(Unparser.unparse(original))
    assert(result == original)

end TestUnparser