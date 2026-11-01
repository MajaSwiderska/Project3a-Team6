package edu.luc.cs.laufer.cs371.expressions

/** An initial algebra of arithmetic expressions. */
enum Expr derives CanEqual:
  case Constant(value: Int)
  case Variable(name: String) //identifier reference
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)

enum Stmt derives CanEqual:
  case ExprStmt(expr: Expr) //expression ";"
  case Assign(target: Expr.Variable, expr: Expr)  //indent "=" expr ";"
  case Cond(guard: Expr, thenBranch: Block, elseBranch: Block)  //if (e) block [else block]
  case Loop(guard: Expr, body: Block) //while (e) block
  case Block(stmts: Stmt*)  // { stmt* }

type Program = Stmt.Block