package parser

import scala.util.parsing.combinator.RegexParsers
import typecheck.AST._

// TODO: the top level lambdas need to be defs, not just raw lambdas.
object Parser extends RegexParsers {

  /**
   * Expression Parsers
   */

  val NUM  = """[1-9][0-9]*""".r  ^^ { s => Lit(Num (s.toInt)) }
  val BOOL = ("true" | "false")   ^^ { s => Lit(Bool(s.toBoolean)) }
  val TERM_ID   = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9]|')*""".r ^^ { s => Name(s) }
  // TODO: these are kind of bogus...figure out a better way.
  val OP   = ("+" | "-" | "*" | "/" | "==")  ^^ { s => Name(s) }
  //(x y z -> (+ (+ x y) z)) = (x -> (y -> (z -> (+ (+ x y) z))))
  def LAM  = parens((TERM_ID+) ~ "->" ~ EXPR) ^^ { case ids ~ "->" ~ exp =>
    ids.init.foldRight(Lam(ids.last, exp)){ case (nextId, fun) => Lam(nextId, fun) }
  }
  //(f a b c d) = ((((f a) b) c) d)
  def APP  = parens(EXPR ~ (EXPR+)) ^^ { case f ~ args =>
    args.tail.foldLeft(App(f, args.head)){ case (fun, nextArg) => App(fun, nextArg)}
  }
  def CONS_APP: Parser[ConstructorApp] = parens(CONS_ID ~ (EXPR*)) ^^ {
    case name ~ exps => ConstructorApp(name, exps)
  }
  def EXPR: Parser[Exp] = BOOL | NUM  | TERM_ID | OP | LAM | APP | CONS_APP
  def DEF: Parser[Def] = parens("def" ~> TERM_ID ~ LAM) ^^ { case id ~ lam => Def(id, lam) }
  val CONS_ID: Parser[Name]     = """[A-Z]([a-zA-Z0-9]|_[a-zA-Z0-9]|')*""".r ^^ { s => Name(s) }
  val CONS: Parser[Constructor] = parens(CONS_ID ~ opt(parens(TYPE*))) ^^ {
    case name ~ args => Constructor(name, args.getOrElse(Nil))
  }
  def DATA = parens("data" ~> CONS_ID ~ (TYVAR*) ~ (CONS*)) ^^ { case name ~ tvs ~ cons => DataDef(name, tvs, cons)}
  def PROG: Parser[Program] = (DEF*) ~ EXPR ^^ { case defs ~ exp => Program(defs, exp) }

  def parseExp(s:String): Either[String, Program] = parse(PROG, s) match {
    case Success(result, _) => Right(result)
    case f@Failure(msg, _)  => Left(msg)
  }

  /**
   * Type Parsers
   */

  val INTT   = "Int".r  ^^ { _ => IntT }
  val BOOLT  = "Bool".r ^^ { _ => BoolT }
  val TYVAR  = """'[a-z]([0-9])*""".r ^^ { s => TyVar(s.drop(1)) }
  //('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2"
  //(('t1 -> 't2) -> 't1 -> 't2) -> 't2
  def TYLAM  = ((INTT | BOOLT | TYVAR) ~ "->" ~ TYPE) ^^ { case in ~ "->" ~ out => TyLam(in, out) }
  def TYLAM_PARENS: Parser[TyLam] = parens(TYLAM | TYLAM_PARENS) ~ opt("->" ~> TYPE) ^^ {
    case in ~ maybeOut => maybeOut.map(TyLam(in, _)).getOrElse(in)
  }
  def TYPE: Parser[Type] = TYLAM_PARENS | TYLAM | INTT | BOOLT | TYVAR
  def parseType(s:String): Either[String, Type] = parse(TYPE, s) match {
    case Success(result, _) => Right(result)
    case f@Failure(msg, _)  => Left(msg)
  }

  def parens[T](p:Parser[T]):Parser[T] = "(" ~> p <~ ")"
}
