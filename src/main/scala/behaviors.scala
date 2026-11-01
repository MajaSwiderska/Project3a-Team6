package edu.luc.cs.laufer.cs371.expressions

import util.Try
import org.json4s.JsonAST.{JValue, JObject, JArray}
import Expr.*
import Stmt.*

object behaviors:

  private def evaluateR(e: Expr): Int = e match
    case Constant(c) => c
    case Variable(n) => throw new IllegalArgumentException(s"unbound variable: $n")
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r) // Seq(l, r).map(evaluateR).sum
    case Minus(l, r) => evaluateR(l) - evaluateR(r) // Seq(l, r).map(evaluateR).foldLeft(0)(_ - _)
    case Times(l, r) => evaluateR(l) * evaluateR(r) // Seq(l, r).map(evaluateR).product
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)

  def evaluate(e: Expr): Try[Int] = Try(evaluateR(e))

  def size(e: Expr): Int = e match
    case Constant(c) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r) // 1 + Seq(l, r).map(size).sum
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)

  def height(e: Expr): Int = e match
    case Constant(c) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))  // 1 + Seq(l, r).map(height).max
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))

  def toJson(e: Expr): JValue = e match
    case Constant(c) => c
    case Variable(n) => "Variable" -> n
    case UMinus(r)   => e.productPrefix -> toJson(r)
    case p           => p.productPrefix -> (0 until p.productArity).map(i => toJson(p.productElement(i).asInstanceOf[Expr]))

  def toJson(s: Stmt): JValue = s match
    case ExprStmt(e) => "ExprStmt" -> toJson(e)
    case Assign(variable(n), e) => "Assign" -> ("Variable" -> n) -> toJson(e)
    case Cond(g, t, e) =>
      JObject(
        "Cond" -> JObject(
          "guard" -> toJson(g),
          "then"  -> toJson(t),
          "else"  -> toJson(e)
        )
      )
    case Loop(g, b) =>
      JObject(
        "Loop" -> JObject(
          "guard" -> toJson(g),
          "body"  -> toJson(b)
        )
      )
    case Block(ss*) => "Block" -> JArray(ss.map(toJson(_): JValue))

  def toJson(p: Program): JValue = Json(p: Stmt)

end behaviors
