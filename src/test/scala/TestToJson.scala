package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import org.json4s.JsonAST.*
import behaviors.toJson
import Expr.*
import Stmt.*

class TestToJson extends AnyFunSuite:

  test("Constant to JSON"):
    assert(toJson(Constant(42)).toString == JInt(42).toString)

  test("Variable to JSON"):
    assert(toJson(Variable("x")).toString ==
      JObject("Variable" -> JString("x")).toString)

  test("Plus to JSON"):
    assert(toJson(Plus(Constant(1), Constant(2))).toString ==
      JObject("Plus" -> JArray(List(JInt(1), JInt(2)))).toString)

  test("UMinus to JSON"):
    assert(toJson(UMinus(Constant(3))).toString ==
      JObject("UMinus" -> JInt(3)).toString)

  test("Assign to JSON"):
    assert(toJson(Assign(Variable("x"), Constant(5))).toString ==
      JObject("Assign" -> JObject(
        "Variable" -> JString("x"),
        "expr"     -> JInt(5)
      )).toString)

  test("Block to JSON is a JArray of statements"):
    assert(toJson(Block(Assign(Variable("x"), Constant(5)))).toString ==
      JObject("Block" -> JArray(List(
        JObject("Assign" -> JObject(
          "Variable" -> JString("x"),
          "expr"     -> JInt(5)
        ))
      ))).toString)

  test("Cond to JSON has guard/then/else keys"):
    val c = Cond(Constant(1), Block(), Block())
    val s = toJson(c).toString
    assert(s.contains("Cond"))
    assert(s.contains("guard"))
    assert(s.contains("then"))
    assert(s.contains("else"))

end TestToJson