package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.* 
import Stmt.* 

//So the reason why the expression grammar is duplicated here instead of importing ASTBuilder is because eacj JavaTokenParsers instance is independent
//I can't compose Parsers from two different instances, I tried it and it doesn't compile so I redefined the small expression grammar locally
object StatementParser extends JavaTokenParsers:
    //Needed so `==` on `~` values compiles with strictEquality
    given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

    //Can't use "if"/"else"/"while" as variable names or else `if (1) {...}` would get parsed as a varaible called `if`.
    //`^?` means parse ident if this predicate holds
    private val reserved = Set("if", "else", "while")
    private def identifier: Parser[String] = 
        ident ^? { case s if !reserved.contains(s) => s }

    //Expression grammar
    //expression ::= term {{ "+" | "-" } term }*
    //`rep` matches zero or more, then foldLeft builds the left associative chain the grammar implies
    def expression: Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^
        { case first ~ rest => rest.foldLeft(first) {
            case (acc, "+" ~ t) => Plus(acc, t)
            case (acc, "-" ~ t) => Minus(acc, t)
        } }

    //term ::= factor { { "*" | "/" | "%" } factor }*
    def term: Parser[Expr] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^
        { case first ~ rest => rest.foldLeft(first) {
            case (acc, "*" ~ f) => Times(acc, f)
            case (acc, "/" ~ f) => Div(acc, f)
            case (acc, "%" ~ f) => Mod(acc, f)
        } }

    //factor ::= wholeNumber | ident| "+" factor | "-" factor | "(" expression ")"
    //`~>` keeps only the right result(drops the "+"), `<~` keeps only the left (drops the ")"). Useful for ignoring punctuations
    def factor: Parser[Expr] = 
        wholeNumber ^^ (s => Constant(s.toInt))
        | identifier ^^ (s => Variable(s))
        | "+" ~> factor
        | "-" ~> factor ^^ (e => UMinus(e))
        | "(" ~> expression <~ ")"

    //Statments
    //block ::= "{" statement* "}"
    //`ss*` unpacks the list into varargs so `Block(stmts: Stmt*)` gets them
    def block: Parser[Block] =
        "{" ~> rep(statement) <~ "}" ^^ (ss => Block(ss*))

    //assignment ::= ident "=" expressions ";"
    def assignment: Parser[Assign] =
        (identifier <~ "=") ~ (expression <~ ";") ^^
            { case name ~ e => Assign(Variable(name), e) }

    //conditional ::= "if" "(" expression ")" block [ "else" block] `opt` gives Some(block) if else is there, none if not
    //Missing else becomes an empty Block() so every Cond has two branches
    def conditional: Parser[Cond] =
        ("if" ~ "(" ~> expression <~ ")") ~ block ~ opt("else" ~> block) ^^
            { case g ~ t ~ e => Cond( g, t, e.getOrElse(Block())) }

    //loop ::= "while" "(" expression ")" block
    def loop: Parser[Loop] = 
        ("while" ~ "(" ~> expression <~ ")") ~ block ^^ { case g ~ b => Loop(g, b) }

    //statement ::= assignment | conditional | loop | block | expression ";"
    //important order: assignment first, otherwise `x = 5;` gets parsed as ExprSStmt(Variable("x")) and then chokes on the `=`
    //`|` tries alternatives left to right so if assignment fails it falls through
    def statement: Parser[Stmt] =
        assignment
        | conditional
        | loop
        | block
        | (expression <~ ";") ^^ (e => ExprStmt(e))

    //program ::= statement* (a block without braces)
    def program: Parser[Program] =
        rep(statement) ^^ (ss => Block(ss*))

end StatementParser

