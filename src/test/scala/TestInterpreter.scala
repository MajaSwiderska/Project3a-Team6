package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable
import scala.util.Try

import Expr.*
import Stmt.*
import Value.*

class TestInterpreter extends AnyFunSuite:

  private def parse(source: String): Program =
    StatementParser.parseAll(StatementParser.program, source).get

  private def freshStore: Store = mutable.Map.empty[String, Value]

  test("integer expressions evaluate to their values") {
    val store = freshStore
    val statement = ExprStmt(Plus(Constant(2), Times(Constant(3), Constant(4))))
    assert(MiniJsRepl.interpret(statement, store) == Num(14))
  }

  test("variable expressions read values from the store") {
    val store = freshStore
    store("x") = Num(12)
    assert(MiniJsRepl.interpret(ExprStmt(Variable("x")), store) == Num(12))
  }

  test("reading an undefined variable fails") {
    val store = freshStore
    val result = Try(MiniJsRepl.interpret(ExprStmt(Variable("missing")), store))
    assert(result.isFailure)
    assert(result.failed.get.isInstanceOf[NoSuchFieldException])
  }

  test("assignment creates a previously unknown variable") {
    val store = freshStore
    val result = MiniJsRepl.interpret(Assign(Variable("x"), Constant(7)), store)
    assert(result == Num(0))
    assert(store("x") == Num(7))
  }

  test("assignment updates an existing variable") {
    val store = freshStore
    store("x") = Num(2)
    val result = MiniJsRepl.interpret(Assign(Variable("x"), Constant(9)), store)
    assert(result == Num(0))
    assert(store("x") == Num(9))
  }

  test("a block returns the value of its last expression") {
    val store = freshStore
    val program = parse("{ x = 7; x; }")
    assert(MiniJsRepl.interpret(program, store) == Num(7))
    assert(store("x") == Num(7))
  }

  test("an empty block evaluates to the void value") {
    assert(MiniJsRepl.interpret(Block(), freshStore) == Num(0))
  }

  test("a conditional executes the true branch") {
    val store = freshStore
    val program = parse("if (1) { x = 4; x; } else { x = 9; x; }")
    assert(MiniJsRepl.interpret(program, store) == Num(4))
    assert(store("x") == Num(4))
  }

  test("a conditional executes the false branch when its guard is zero") {
    val store = freshStore
    val program = parse("if (0) { x = 4; x; } else { x = 9; x; }")
    assert(MiniJsRepl.interpret(program, store) == Num(9))
    assert(store("x") == Num(9))
  }

  test("a conditional without an else branch evaluates to void when false") {
    val store = freshStore
    val program = parse("if (0) { x = 4; }")
    assert(MiniJsRepl.interpret(program, store) == Num(0))
    assert(!store.contains("x"))
  }

  test("a while loop repeats until its guard becomes zero") {
    val store = freshStore
    val program = parse("x = 0; while (x - 3) { x = x + 1; } x;")
    assert(MiniJsRepl.interpret(program, store) == Num(3))
    assert(store("x") == Num(3))
  }

  test("a loop itself evaluates to the void value") {
    val store = freshStore
    val program = parse("x = 0; while (x - 2) { x = x + 1; }")
    assert(MiniJsRepl.interpret(program, store) == Num(0))
    assert(store("x") == Num(2))
  }

  test("nested blocks and imperative statements work together") {
    val store = freshStore
    val program = parse("{ x = 2; { y = 3; x * y; } }")
    assert(MiniJsRepl.interpret(program, store) == Num(6))
    assert(store("x") == Num(2))
    assert(store("y") == Num(3))
  }

  test("the top-level interpreter failure can be captured without crashing") {
    val result = Try(MiniJsRepl.interpret(parse("unknown;"), freshStore))
    assert(result.isFailure)
  }

end TestInterpreter