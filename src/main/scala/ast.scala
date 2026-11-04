package edu.luc.cs.laufer.cs371.expressions

//Expressions: the arithmetic side of MiniJS
//Each case is one kind of expression the parser can build
//It derives CanEqual is needed because the project uses strictEquality
enum Expr derives CanEqual:
  case Constant(value: Int) //a number like 42
  case Variable(name: String) //a name like x or y2
  case UMinus(expr: Expr) //unary minus, -x
  case Plus(left: Expr, right: Expr)  //a + b
  case Minus(left: Expr, right: Expr) //a - b
  case Times(left: Expr, right: Expr) //a * b
  case Div(left: Expr, right: Expr) //a / b
  case Mod(left: Expr, right: Expr) //a % b

//Statements: the imperative stuff(assign, if, while, blocks)
//Statements are seperate from expressions because they don't produce a value, they just do something
//Block is a Stmt itself so if/while bodies (which are blocks) and bare{...} statements can be represented the same way
//It also lets us next blocks inside blocks with no extra case
enum Stmt derives CanEqual:
  case ExprStmt(expr: Expr) //expression ";"
  case Assign(target: Expr.Variable, expr: Expr)  //"x = expr ;"
  case Cond(guard: Expr, thenBranch: Block, elseBranch: Block)  //if (e) block [else block]
  case Loop(guard: Expr, body: Block) //while (e) block
  case Block(stmts: Stmt*)  // { stmt* }

//A program is just a lists of statements, a block without the braces
//Making it an alias instead of a new case can keep everything reusable
type Program = Stmt.Block