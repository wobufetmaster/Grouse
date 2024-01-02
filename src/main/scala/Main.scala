import scala.io.Source
import scala.io.Source._
import scala.io.StdIn.readLine
import scala.util.{Try, Using, Success, Failure}
import scala.annotation.tailrec

import scala.util.parsing.combinator._

import cats.parse.{Parser => P, Parser0, Numbers}
import cats.parse.Rfc5234.{sp, alpha, digit}




  

object arithmeticParser:
  sealed trait arith
  case class literal(num: Int) extends arith
  case class addition(op1: arith, op2: arith) extends arith
  case class mult(op1: arith, op2: arith) extends arith

  val whitespace: P[Unit] = P.charsWhile(c => c.isWhitespace).void

  val numParser: P[arith] = Numbers.digits.map(s => literal(s.toInt))

  //val multParser: P[arith] = P.char('*')



def readFileAsString(filePath: String): Option[String] =
  Try(Using(Source.fromFile(filePath))(_.mkString).get).toOption

@main def main(script: String*): Unit =
  script match
    case Seq() => run_repl()
    case Seq(script_path) => readFileAsString(script_path) match
      case Some(source) => run_file(source)
      case None => println("File not found!")
    case _ => println("Too many arguments!")

def run_repl(): Unit = 
  println("Running REPL")
  while true do
    val line = readLine("Grouse> ")
    if line == null || line == "exit" then 
      println("All done!")
      return None
    execute(line) match
      case Success(_) => ()
      case Failure(e) => println(e.getMessage)

def execute(source: String): Try[String] =
  Parser.parse(source)
  //val tokens = Parser.parse(source)
  // tokens match
  //   case Left(err) => println(err)
  //   case Right(tokens) => println(tokens)
  Failure(new Exception("Not implemented"))


def run_file(source: String): Try[String] =
  execute(source)
  Success("Success")