package typecheck

// TODO: make these tests run against all the type checkers, where possible.
object TypeCheckerTests extends org.scalacheck.Properties("TypeChecker") with util.Compare {

  typeCheck("7", "Int")
  typeCheck("true", "Bool")
  typeCheck("false", "Bool")
  typeCheck("(identity 7)", "Int")
  typeCheck("(== 7 8)", "Bool")
  typeCheck("(+ 7 8)", "Int")
  typeCheck("(- 7 8)", "Int")

  // test functions
  typeCheck("(x -> (+ 5 5))", "'t0 -> Int")
  typeCheck("(x -> (+ x 5))", "Int -> Int")
  typeCheck("(x y z -> (+ x 5))", "Int -> 't0 -> 't1 -> Int")
  typeCheck("(x y z -> (== z y))", "'t0 -> Int -> Int -> Bool")
  typeCheck("(x y z -> (== z (+ x y)))", "Int -> Int -> Int -> Bool")

  // f = t0 -> t1, g = t1 -> t2, x = t0
  typeCheck("(f g x -> (g (f x)))", "('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2")

  def typeCheck(code: String, expectedType:String) =
    compare (code, TypeChecker.typeCheck(code), parser.Parser.parseType(expectedType))
}
