package edu.luc.cs.laufer.cs371.expressions

import scala.io.Source
import StatementParser.* 
import behaviors.*
import org.json4s.native.JsonMethods.{pretty, render}

//Read eval print loop
//For each line: parse, print AST, print JSON, print unparsed source
//No evaluation is 3a - thats 3b I think
object MiniJsRepl:
    //Process one line of input (or one shell arg)
    def process(input: String): Unit = 
        println("You entered: " + input)
        //parseAll builds a Program from the full input
        // Success = parsed, Failure = recoverable error, Error = unrecoverable
        parseAll(program, input) match
            case Success(ast, _) =>
                println("The parsed statement are:")
                println(ast)    //toString on the case class = the AST tree
                //pretty(render(jvalue)) = indented JSON, without pretty it would be one long line
                println("The corresponding JSON structure is:")
                println(pretty(render(toJson(ast))))
                println("The unparsed statements are:")
                println(Unparser.unparse(ast))
            case Failure(msg, _) => println("Couldn't parse: " + msg)
            case Error(msg, _)   => println("Parse error: " + msg)

    //If given an argument, it should be treated as a one shot program
    //Otherwise, read from srdin line by line
    def main(args: Array[String]): Unit =
        if args.nonEmpty then
            process(args.mkString(" "))
        else 
            Source.stdin.getLines().foreach { line => 
                process(line) 
                print("minijs> ")   //prompt after processing
            }
end MiniJsRepl
