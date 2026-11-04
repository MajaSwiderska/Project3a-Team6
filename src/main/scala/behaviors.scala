package edu.luc.cs.laufer.cs371.expressions

import util.Try
import org.json4s.JsonAST.{JValue, JObject, JString, JArray, JInt}
import Expr.*
import Stmt.*

//Everything that "does something" with the AST: evaluate, size, height, toJson
//All of them are structural recursion: case on the shape recurse on the children combine
object behaviors:
  
  //Evaluate (expressions only, project 3a has no enviroment)
  //Private helper that can throw, the prublic `evaluate` wraps it in Try
  private def evaluateR(e: Expr): Int = e match
    case Constant(c) => c
    //No environment means no variable is an error, try turn this into Failure so the REPL won't crash
    case Variable(n) => throw new IllegalArgumentException(s"unbound variable: $n")
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r)
    case Minus(l, r) => evaluateR(l) - evaluateR(r)
    case Times(l, r) => evaluateR(l) * evaluateR(r)
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)

  def evaluate(e: Expr): Try[Int] = Try(evaluateR(e))

  //Size: number of AST nodes
  def size(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)

  //Height: longest root to leaf path, leaves counts as 1
  def height(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))

  //JSON for expressions
  def toJson(e: Expr): JValue = e match
    case Constant(c) => JInt(c)
    //Wrap the variable name in an object so it isn't confused with a string constant
    case Variable(n) => JObject("Variable" -> JString(n))
    //Unary case: {"UMinus": inner}
    case UMinus(r)   => JObject(e.productPrefix -> toJson(r))
    //n-ary case: {"Plus": [child1, child2]} etc
    //productPrefic gives the case name, productElement(i) gives children
    case p =>
      JObject(p.productPrefix -> JArray(
        (0 until p.productArity).map(i =>
          toJson(p.productElement(i).asInstanceOf[Expr])).toList
      ))

  //JSON for statements
  def toJson(s: Stmt): JValue = s match
    case ExprStmt(e) =>
      JObject("ExprStmt" -> toJson(e))
    case Assign(Variable(n), e) =>
      JObject("Assign" -> JObject("Variable" -> JString(n), "expr" -> toJson(e)))
    case Cond(g, t, e) =>
      JObject("Cond" -> JObject(
        "guard" -> toJson(g),
        "then"  -> toJson(t),
        "else"  -> toJson(e)
      ))
    case Loop(g, b) =>
      JObject("Loop" -> JObject(
        "guard" -> toJson(g),
        "body"  -> toJson(b)
      ))
    //Block -> JSON array of its statements
    case Block(ss*) =>
      JObject("Block" -> JArray(ss.map(toJson(_)).toList))

  //Program is a block so just delegate
  def toJson(p: Program): JValue = toJson(p: Stmt)

end behaviors