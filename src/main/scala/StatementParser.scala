package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.* 
import Stmt.* 

object StatementParser extends JavaTokenParsers:
    given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

    private val reserved = Set("if", "else", "while")
    private def identifier: Parser[String] = 
        ident ^? { case s if !reserved.contains(s) =? s }

    def expressions: Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^
        { case first ~ rest => rest.foldLeft(first) {
            case (acc, "+" ~ t) => Plus(acc, t)
            case (acc, "-" ~ t) => Minus(acc, t)
        } }

    def term: Parser[Expr] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^
        { case first ~ rest => rest.foldLeft(first) {
            case (acc, "*" ~ f) => Times(acc, f)
            case (acc, "/" ~ f) => Div(acc, f)
            case (acc, "%" ~ f) => Mod(acc, f)
        } }

    def factor: Parser[Expr] = 
        wholeNumber ^^ (s => Constant(s.toInt))
        | identifier ^^ (s => Variable(s))
        | "+" ~> factor
        | "-" ~> factor ^^ (e => UMinus(e))
        | "(" ~> expressions <~ ")"

    def block: Parser[Block] =
        "{" ~> rep(statement) <~ "}" ^^ (ss => Block(ss*))

    def assignment: Parser[Assign] =
        (identifier <~ "=") ~ (expressions <~ ";") ^^
            { case name ~ e => Assign(Variable(name), e) }

    def conditional: Parser[Cond] =
        ("if" ~ "(" ~> expression <~ ")") ~ block ~ opt("else" ~> block) ^^
            { case g ~ t ~ e => Cond( g, t, e.getOrElse(Block())) }

    def loop: Parser[Loop] = 
        ("while" ~ "(" ~> expression <~ ")") ~ block ^^ { case g ~ b => Loop(g, b) }

    def statement: Parser[Stmt] =
        assignment
        | conditional
        | loop
        | block
        | (expressions <~ ";") ^^ (e => ExprStmt(e))

    def program: Parser[Program] =
        rep(statement) ^^ (ss => Block(ss*))

end StatementParser

