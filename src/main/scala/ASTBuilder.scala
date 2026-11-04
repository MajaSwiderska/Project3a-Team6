package edu.luc.cs.laufer.cs371.expressions

import Expr.*

//Concrete version of ExprParser[Expr]: each `on*` methid builds a node
//Used directly for plain numeric expressions and can also handle variables if you parse with ASTBuilder.expr
//StatementParser uses its own expression grammar
object ASTBuilder extends ExprParser[Expr]:

  //"term (+|-) term" becomes Plus or Minus
  //None means there was no operator after the first term
  override def onExpr: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("+" ~ r) => Plus(l, r)
    case l ~ Some("-" ~ r) => Minus(l, r)

  //Same idea for * / %
  override def onTerm: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("*" ~ r) => Times(l, r)
    case l ~ Some("/" ~ r) => Div(l, r)
    case l ~ Some("%" ~ r) => Mod(l, r)
  
  //"42" -> Constant(42). Compose toInt with Constant.apply
  override def onNumber = Constant.apply compose (_.toInt)

  //New: an identifier in a factor becomes a Variable reference
  override def onVariable = Variable.apply

  override def onPlusFactor = identity  //+ does nothing

  override def onMinusFactor = UMinus.apply // - wraps in UMinus

  override def onParenExpr = identity //parents don't change the AST

end ASTBuilder
