package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*

//Grammar for expressions (with variables added)
//This trait has the grammar but leaves the "what should i build?" part abstracted
//ASTBuilder builds Exprs, RawBuilder builds any, same grammar, different output type(thats the result parameter)
trait ExprParser[Result] extends JavaTokenParsers:
  //I need this so `==` between `~` results compiles under strict equality
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Result] = term ~! opt(("+" | "-") ~ term) ^^ onExpr

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Result] = factor ~! opt(("*" | "/" | "%") ~ factor) ^^ onTerm

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  //Added `ident` so that `x` in an expression becomes Variable(x) instead of failing to parse
  def factor: Parser[Result] = 
    wholeNumber ^^ onNumber
    | ident ^^ onVariable //NEW
    | "+" ~> factor ^^ onPlusFactor //unary + is a no-op
    | "-" ~> factor ^^ onMinusFactor//unary - wraps in UMinus
    | "(" ~> expr <~ ")" ^^ onParenExpr //parens just group

  //Subclass implement these to build whatever they want
  def onExpr: Result ~ Option[String ~ Result] => Result
  def onTerm: Result ~ Option[String ~ Result] => Result
  def onNumber: String => Result
  def onVariable: String => Result  //New for variable
  def onPlusFactor: Result => Result
  def onMinusFactor: Result => Result
  def onParenExpr: Result => Result
  
end ExprParser
