package edu.luc.cs.laufer.cs371.expressions

import scala.io.Source
import StatementParser.* 
import behaviors.* 
import org.json4.native.JsonMethods. {pretty, render}

object MiniJsRepl:
    def process(input: String): Unit = 
        println("You entered: " + input)
        parseAll(program, input) match
            case Success(ast, _) =>
                println("The parsed statement are:")
                println(ast)
                println("The corresponding JSON structure is:")
                println(pretty(render(toJson(ast))))
                println("The unparsed statements are:")
                println(Unparser.unparse(ast))
            case Failure(msg, _) => println("Couldn't parse: " + msg)
            case Error(msg, _)   => println("Parse error: " + msg)

    def main(args: Array[String]): Unit =
        if args.nonEmpty then
            process(args.mkString(" "))
        else 
            Source.stdin.getLines().foreach { line => 
                process(line) 
                print("minijs> ")
            }
end MiniJsRepl
