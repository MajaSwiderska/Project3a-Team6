package edu.luc.cs.laufer.cs371.expressions

//Keeps the raw combinator result instead of building an AST
//Useful for showing the untyped parse tree in Calculators output
//Must overirde all 7 hooks or the compiler yells "object creation impossible"
object RawBuilder extends ExprParser[Any]:
  override def onExpr = identity
  override def onTerm = identity
  override def onNumber = identity
  override def onVariable = identity  //pass the identifer through as-is
  override def onPlusFactor = identity
  override def onMinusFactor = identity
  override def onParenExpr = identity

end RawBuilder
