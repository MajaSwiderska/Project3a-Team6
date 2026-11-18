package edu.luc.cs.laufer.cs371.expressions

import scala.io.Source
import scala.collection.mutable
import scala.util.Try
import StatementParser.*
import Expr.*
import Stmt.*
import Value.*

import behaviors.{evalExpr, toJson}
import org.json4s.native.JsonMethods.{pretty, render}

// Read-eval-print loop for MiniJS.
object MiniJsRepl:

    // Interpret statements using the mutable store.
    // Expressions produce their integer value; assignments and loops are void.
    // A block produces the value of its last statement.
    def interpret(statement: Stmt, store: Store): Value = statement match
        case ExprStmt(expr) =>
            Num(evalExpr(expr, store))

        case Assign(Variable(name), expr) =>
            val value = evalExpr(expr, store)
            store(name) = Num(value)
            Num(0)

        case Cond(guard, thenBranch, elseBranch) =>
            if evalExpr(guard, store) != 0 then
                interpret(thenBranch, store)
            else
                interpret(elseBranch, store)

        case Loop(guard, body) =>
            while evalExpr(guard, store) != 0 do
                interpret(body, store)
                ()
            Num(0)

        case Block(statements*) =>
            var result: Value = Num(0)
            for current <- statements do
                result = interpret(current, store)
            result

    // Process one line of input (or one shell argument).
    def process(input: String): Unit =
        println("You entered: " + input)

        val store: Store = mutable.Map.empty
        println("Memory: " + store)

        // Parse the complete input as a program.
        parseAll(program, input) match
            case Success(ast, _) =>
                println("The parsed statements are:")
                println(ast)
                println("The corresponding JSON structure is:")
                println(pretty(render(toJson(ast))))
                println("The unparsed statements are:")
                println(Unparser.unparse(ast))

                // Keep interpretation failures from terminating the REPL.
                val tryResult = Try(interpret(ast: Stmt, store))
                println("It evaluates to " + tryResult)
                println("Memory: " + store)

            case Failure(msg, _) => println("Couldn't parse: " + msg)
            case Error(msg, _)   => println("Parse error: " + msg)

    // If given an argument, treat it as a one-shot program.
    // Otherwise, read standard input line by line.
    def main(args: Array[String]): Unit =
        if args.nonEmpty then
            process(args.mkString(" "))
        else
            print("minijs> ")
            Source.stdin.getLines().foreach { line =>
                process(line)
                print("minijs> ")
            }
end MiniJsRepl