package edu.luc.cs.laufer.cs371.expressions

import Expr.*
import Stmt.*

object Unparser:

  def unparse(program: Program): String =
    program.stmts.map(stmt => renderStmt(stmt, 0)).mkString("\n")

  private def spaces(indent: Int): String =
    " " * indent

  def unparseExpr(expr: Expr): String =
    renderExpr(expr)

  private def renderExpr(expr: Expr): String =
    expr match
      case Constant(value) if value < 0 =>
        s"($value)"

      case Constant(value) =>
        value.toString

      case Variable(name) =>
        name

      case UMinus(inner) =>
        s"(-${renderExpr(inner)})"

      case Plus(left, right) =>
        s"(${renderExpr(left)} + ${renderExpr(right)})"

      case Minus(left, right) =>
        s"(${renderExpr(left)} - ${renderExpr(right)})"

      case Times(left, right) =>
        s"(${renderExpr(left)} * ${renderExpr(right)})"

      case Div(left, right) =>
        s"(${renderExpr(left)} / ${renderExpr(right)})"

      case Mod(left, right) =>
        s"(${renderExpr(left)} % ${renderExpr(right)})"

  private def renderStmt(stmt: Stmt, indent: Int): String =
    stmt match
      case ExprStmt(expr) =>
        spaces(indent) + renderExpr(expr) + ";"

      case Assign(Variable(name), expr) =>
        spaces(indent) + s"$name = ${renderExpr(expr)};"

      case Cond(guard, thenBranch, elseBranch) =>
        val prefix = spaces(indent) + s"if (${renderExpr(guard)}) "
        val thenText = renderBlockContents(thenBranch, indent)

        if elseBranch.stmts.isEmpty then
          prefix + thenText
        else
          prefix + thenText + " else " +
            renderBlockContents(elseBranch, indent)

      case Loop(guard, body) =>
        spaces(indent) +
          s"while (${renderExpr(guard)}) " +
          renderBlockContents(body, indent)

      case block: Block =>
        spaces(indent) + renderBlockContents(block, indent)

  private def renderBlockContents(
                                   block: Block,
                                   indent: Int
                                 ): String =
    val body = block.stmts
      .map(stmt => renderStmt(stmt, indent + 2))
      .mkString("\n")

    if body.isEmpty then
      "{\n" + spaces(indent) + "}"
    else
      "{\n" + body + "\n" + spaces(indent) + "}"

end Unparser