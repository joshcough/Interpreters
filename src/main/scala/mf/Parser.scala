package mf

import scala.util.parsing.combinator.RegexParsers
import AST._

// TODO: the top level lambdas need to be defs, not just raw lambdas.
object Parser {

  private object Inner extends RegexParsers {

    /**
     * Expression Parsers
     */

    val NUM  = """[1-9][0-9]*""".r  ^^ { s => Lit(Num (s.toInt)) }
    val LIT = NUM // TODO: add chars, strings? doubles?
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
    def EXPR: Parser[Exp] = LIT | TERM_ID | OP | LAM | APP | CONS_APP | CONS_ID

    val CONS_ID: Parser[Name]     = """[A-Z]([a-zA-Z0-9]|_[a-zA-Z0-9]|')*""".r ^^ { s => Name(s) }
    val CONS: Parser[Constructor] = parens(CONS_ID ~ opt(TYPE*)) ^^ {
      case name ~ args => Constructor(name, args.getOrElse(Nil))
    }
    val DATA = parens("data" ~> CONS_ID ~ parens(TYVAR*) ~ parens(CONS+)) ^^
      { case name ~ tvs ~ cons => DataDef(name, tvs, cons)}
    def VAL: Parser[Val] = parens("val" ~> TERM_ID ~ EXPR) ^^ { case id ~ exp => Val(id, exp) }
    def DEF: Parser[Val] = parens("def" ~> TERM_ID ~ LAM)  ^^ { case id ~ lam => Val(id, lam) }

    def DECL = DATA | VAL | DEF
    def PROG = (DECL+) ^^ { case decls => Program(decls) }

    def parseExpr(s:String)    = parseToEither(s, EXPR)
    def parseProgram(s:String) = parseToEither(s, PROG)

    /**
     * Type Parsers
     */

    val INTT   = "Int".r  ^^ { _ => IntT }
    val LITT   = INTT
    /**
     * important case to consider: Maybe (Maybe 't0) -> Int
     * this is really (Maybe (Maybe 't0)) -> Int
     * see ParserTests for more details.
     */
    val TYCON_BODY  = (CONS_ID ~ ((LITT | TYVAR | parens(TYPE))*)) ^^ { case s ~ args => TyCon(s, args) }
    val TYCON = parens(TYCON_BODY) | TYCON_BODY
    val TYVAR  = """'[a-z]([0-9])*""".r ^^ { s => TyVar(s.drop(1)) }
    /**
     * Important tricky cases to consider:
     * /('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2"
     * (('t1 -> 't2) -> 't1 -> 't2) -> 't2
     */
    def TYLAM  = ((LITT | TYCON | TYVAR) ~ "->" ~ TYPE) ^^ { case in ~ "->" ~ out => TyLam(in, out) }
    def TYLAM_PARENS: Parser[TyLam] = parens(TYLAM | TYLAM_PARENS) ~ opt("->" ~> TYPE) ^^ {
      case in ~ maybeOut => maybeOut.map(TyLam(in, _)).getOrElse(in)
    }
    def TYPE: Parser[Type] = TYLAM_PARENS | TYLAM | TYCON | LITT | TYVAR

    def parseData(s:String) = parseToEither(s, DATA)
    def parseType(s:String) = parseToEither(s, TYPE)

    /**
     * Helper functions
     */

    def parseToEither[T](s:String, p: Parser[T]): Either[String, T] = parse(p, s) match {
      case Success(result, _) => Right(result)
      case f@Failure(msg, _)  => Left(msg)
    }
    def parens[T](p:Parser[T]):Parser[T] = "(" ~> p <~ ")"
  }

  def parseProgram(s:String)  = Inner.parseProgram(s)
  def parseType(s:String)     = Inner.parseType(s)
  def parseData(s:String)     = Inner.parseData(s)
  def parseExpr(s:String)     = Inner.parseExpr(s)
}
