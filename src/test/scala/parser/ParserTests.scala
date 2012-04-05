package parser

object TypeParserTests  extends org.scalacheck.Properties("Type Parser") with util.Compare{

  parseType("Int")
  parseType("Int -> Int")
  parseType("Int -> Bool -> Int")
  parseType("'t0 -> 't0 -> 't0")
  parseType("'t0 -> 't1")
  parseType("'t0")
  parseType("'t0 -> Bool")
  parseType("('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2")
  parseType("(('t1 -> 't2) -> 't1 -> 't2) -> 't2")
  parseType("('t1 -> ('t2 -> 't1) -> 't2) -> 't2")

  def parseType(typeString: String, expectedTypeString:Option[String]=None) = compare(
    typeString,
    Parser.parseType(typeString).right.map(_.toString),
    Right(expectedTypeString.getOrElse(typeString))
  )
}

object ExpressionParserTests  extends org.scalacheck.Properties("Expression Parser") with util.Compare {

  import typecheck.AST._

  val id = Lam(Name("x"), Name("x"))
  val idDef = Def(Name("id"), id)
  
  parseExp("(def id (x -> x)) 7", Program(List(idDef), Lit(Num(7))))
  parseExp("(def id (x -> x)) (x -> x)", Program(List(idDef), id))
  parseExp("(def id (x -> x)) (def id (x -> x)) (x -> x)", Program(List(idDef, idDef), id))
  parseExp("6",          Program(Lit(Num(6))))
  parseExp("( x -> x )", Program(id))
  parseExp("(x->x)",     Program(id))
  parseExp("(x x)",      Program(App(Name("x"),Name("x"))))
  parseExp("(x x')",     Program(App(Name("x"),Name("x'"))))
  parseExp("(x x'')",    Program(App(Name("x"),Name("x''"))))
  parseExp("(x x'p')",   Program(App(Name("x"),Name("x'p'"))))

  def parseExp(code: String, expected: Program) =
    compare(code, Parser.parseExp(code), Right(expected))
}
