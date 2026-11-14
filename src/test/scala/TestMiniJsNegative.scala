package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import StatementParser.* 

class TestMiniJsNegative extends AnyFunSuite:

    private def rejects(source: String): Boolean = 
        !parseAll(program, source).successful

    test("missing semicolon")        (assert(rejects("x = 5")))
    test("unclosed block")           (assert(rejects("if (1) { x = 2;")))
    test("dangling else")            (assert(rejects("else { x = 1; }")))
    test("keyword as variable")      (assert(rejects("if = 5;")))
    test("empty if condition")       (assert(rejects("if () { x = 1; }")))
    test("malformed assignment")     (assert(rejects("x = ")))
    test("unbalanced parens")        (assert(rejects("x = (1 + 2;")))
    test("missing rhs of binary op") (assert(rejects("x = 5 + ;")))
    test("missing block after if")   (assert(rejects("if (1) x = 5;")))
    test("garbage after statement")  (assert(rejects("x = 5; @@@")))

end TestMiniJsNegative
