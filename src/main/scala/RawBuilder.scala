package edu.luc.cs.laufer.cs371.expressions

object RawBuilder extends ExprParser[Any]:
  override def onExpr = identity
  override def onTerm = identity
  override def onNumber = identity
  override def onVariable = identity  //pass the identifer through as-is
  override def onMinusFactor = identity
  override def onParenExpr = identity
end RawBuilder
