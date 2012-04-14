package parser

import scala.util.parsing.combinator.RegexParsers
import typecheck.TypeCheckerWithInferenceAST._
import scalaz.EitherT._
import scalaz.std.either._
import scalaz.syntax.monad._

// TODO: add a type parser.
// TODO: the top level lambdas need to be defs, not just raw lambdas.
object Parser {

  object Inner extends RegexParsers {
    def parens[T](p:Parser[T]):Parser[T] = "(" ~> p <~ ")"

    val NUM  = """[1-9][0-9]*""".r      ^^ { s => Lit(Num (s.toInt)) }
    val BOOL = ("true" | "false")       ^^ { s => Lit(Bool(s.toBoolean)) }
    // TODO: allow ' (prime)... ex: x'
    val ID   = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r ^^ { s => Id(s) }
    // TODO: these are kind of bogus...figure out a better way.
    val OP   = ("+" | "-" | "*" | "/" | "==")  ^^ { s => Id(s) }
    // TODO: allow for multiple argument lambdas
    def LAM  = parens(ID ~ "->" ~ EXPR) ^^ { case id ~ "->" ~ exp => Lam(id, exp) }
    // TODO: allow for application of more than one argument at a time.
    def APP  = parens(EXPR ~ EXPR)      ^^ { case f ~ a => App(f, a) }
    def EXPR: Parser[Exp] = BOOL | NUM  | ID | OP | LAM | APP

    // this is terrible, but whatever, it works for now
    // it allows for a bunch of top level lambdas, and then a final expression.
    // i wanted it to be simply LAM* | EXP, but it didnt work with only lambdas.
    def PROG: Parser[Either[String, Program]] = (EXPR+) ^^ { exps =>
      exps.init.find(! _.isInstanceOf[Lam]).map {
        e => Left("expected Lam, got: " + e)
      }.getOrElse(Right(Program(exps)))
    }

    def run(s:String): Either[String, Program] = parse(PROG, s) match {
      case Success(result, _) => result
      case f@Failure(msg, _)  => Left(msg)
    }
  }

  def parse(s:String): Either[String, Program] = Inner.run(s)

  // tests for parsing this stuff
  // TODO: put these in a real test.
  def main(args:Array[String]) {
    def doParse(s:String) = parse(s) match {
      case Left(err) => sys.error(err)
      case Right(p)  => println(p)
    }
    doParse("(x -> x) 7")
    doParse("(x -> x) (x -> x)")
    doParse("(x -> x) (x -> x)(x -> x)")
    doParse("6")
    doParse("( x -> x )")
    doParse("(x->x)")
    doParse("(x x)")
  }
}
