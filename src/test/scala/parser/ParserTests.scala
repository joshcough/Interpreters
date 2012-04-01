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

  def parseType(typeString: String, expectedTypeString:Option[String]=None) = compare(
    typeString,
    Parser.parseType(typeString).right.map(_.toString),
    Right(expectedTypeString.getOrElse(typeString))
  )
}

object ExpressionParserTests  extends org.scalacheck.Properties("Expression Parser") with util.Compare {

  import typecheck.TypeCheckerWithInferenceAST._

  parseExp("(x -> x) 7", Lam(Id("x"), Id("x")), Lit(Num(7)))
  parseExp("(x -> x) (x -> x)", Lam(Id("x"), Id("x")), Lam(Id("x"), Id("x")))
  parseExp("(x -> x) (x -> x)(x -> x)", Lam(Id("x"), Id("x")), Lam(Id("x"), Id("x")), Lam(Id("x"), Id("x")))
  parseExp("6", Lit(Num(6)))
  parseExp("( x -> x )", Lam(Id("x"), Id("x")))
  parseExp("(x->x)", Lam(Id("x"), Id("x")))
  parseExp("(x x)", App(Id("x"),Id("x")))

  def parseExp(code: String, expectedProgram:Exp*) =
    compare(code, Parser.parse(code), Right(Program(expectedProgram.toList)))
}
