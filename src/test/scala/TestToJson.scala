package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import org.json4s.JsonAST.* 
import behaviors.toJson
import Expr.*
import Stmt.* 

class TestToJson extends AnyFunSuite:

test("Constant to JSON"):
    assert(toJson(Constant(42)) == JInt(42))

test("Variable to JSON"):
    assert(toJson(Variable("x")) == JObject("Variable" -> JString("x")))

test("Plus to JSON"):
    assert(toJson(Plus(Constant(1), Constant(2))) ==
        JObject("Plus" -> JArray(List(JInt(1), JInt(2)))))

test("UMinus to JSON"):
    assert(toJson(UMinus(Constant(3))) == JObject("UMinus" -> JInt(3)))

test("Assign to JSON"):
    assert(toJson(Assign(Variable("x"), Constant(5))) ==
        JObject("Assign" -> JObject(
            "Variable" -> JString("x"),
            "expr"     -> JInt(5)
    )))

test("Block to JSON is a JArray of statements"):
    assert(toJson(Block(Assign(Variable("x"), Constant(5)))) ==
        JObject("Block" -> JArray(List(
            JObject("Assign" -> JObject(
            "Variable" -> JString("x"),
            "expr"     -> JInt(5)
        ))
    ))))

test("Cond to JSON has guard/then/else keys"):
    val c = Cond(Constant(1), Block(), Block())
    val s = toJson(c).toString
    assert(s.contains("\"Cond\""))
    assert(s.contains("\"guard\""))
    assert(s.contains("\"then\""))
    assert(s.contains("\"else\""))

end TestToJson