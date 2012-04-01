package parser

import scala.util.parsing.combinator.RegexParsers
import typecheck.TypeCheckerWithInferenceAST._

// TODO: the top level lambdas need to be defs, not just raw lambdas.
object Parser {

  object ExpressionParser extends RegexParsers {
    def parens[T](p:Parser[T]):Parser[T] = "(" ~> p <~ ")"

    val NUM  = """[1-9][0-9]*""".r      ^^ { s => Lit(Num (s.toInt)) }
    val BOOL = ("true" | "false")       ^^ { s => Lit(Bool(s.toBoolean)) }
    // TODO: allow ' (prime)... ex: x'
    val ID   = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r ^^ { s => Id(s) }
    // TODO: these are kind of bogus...figure out a better way.
    val OP   = ("+" | "-" | "*" | "/" | "==")  ^^ { s => Id(s) }
    //(x y z -> (+ (+ x y) z)) = (x -> (y -> (z -> (+ (+ x y) z))))
    def LAM  = parens((ID+) ~ "->" ~ EXPR) ^^ { case ids ~ "->" ~ exp =>
      ids.init.foldRight(Lam(ids.last, exp)){ case (nextId, fun) => Lam(nextId, fun) }
    }
    //(f a b c d) = ((((f a) b) c) d)
    def APP  = parens(EXPR ~ (EXPR+))     ^^ { case f ~ args =>
      args.tail.foldLeft(App(f, args.head)){ case (fun, nextArg) => App(fun, nextArg)}
    }
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

  object TypeParser extends RegexParsers {
    def parens[T](p:Parser[T]):Parser[T] = "(" ~> p <~ ")"
    val INTT   = "Int".r ^^ { _ => IntT }
    val BOOLT  = "Bool".r ^^ { _ => BoolT }
    val TYVAR  = """'[a-z]([0-9])*""".r ^^ { s => TyVar(s.drop(1)) }
    //('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2"
    //(('t1 -> 't2) -> 't1 -> 't2) -> 't2
    def TYLAM  = ((INTT | BOOLT | TYVAR) ~ "->" ~ TYPE) ^^ { case in ~ "->" ~ out => TyLam(in, out) }
    def TYLAM_PARENS: Parser[TyLam] = parens(TYLAM | TYLAM_PARENS) ~ opt("->" ~> TYPE) ^^ {
      case in ~ maybeOut => maybeOut.map(TyLam(in, _)).getOrElse(in)
    }


    def TYPE: Parser[Type] = TYLAM_PARENS | TYLAM | INTT | BOOLT | TYVAR
    def run(s:String): Either[String, Type] = parse(TYPE, s) match {
      case Success(result, _) => Right(result)
      case f@Failure(msg, _)  => Left(msg)
    }
  }

  def parse(s:String): Either[String, Program] = ExpressionParser.run(s)
  def parseType(s:String): Either[String, Type] = TypeParser.run(s)
}
