package typecheck

import org.scalacheck.Prop._
import org.scalacheck.Properties
import typecheck.TypeCheckerWithInferenceAST._

// TODO: make these tests run against all the type checkers, where possible.
object TypeCheckerTests extends Properties("TypeChecker"){

  def typeCheck(code: String, expected:Type) = {
    property(code) = secure {
      val result = TypeChecker.typeCheck(code)
      if(result != Right(expected)) println("expected: " + Right(expected) + ", got: " + result)
      result == Right(expected)
    }
  }

  typeCheck("7",     numCon)
  typeCheck("true",  boolCon)
  typeCheck("false", boolCon)

  typeCheck("(identity 7)", numCon)
  typeCheck("(== 7 8)", boolCon)

  typeCheck("(+ 7 8)", numCon)
  typeCheck("(- 7 8)", numCon)

  // test functions

  // f = t0 -> t1, g = t1 -> t2, x = t0
  // TODO: looks like im going to need a type parser too.
  typeCheck("(f g x -> (g (f x)))",
    TyLam(
      TyLam(TyVar("t0"),TyVar("t1")),
      TyLam(TyLam(TyVar("t1"),TyVar("t2")),
        TyLam(TyVar("t0"),TyVar("t2")))))

  // (x -> (+ 5 5))
  typeCheck("(x -> (+ 5 5))", TyLam(TyVar("t0"), numCon))
  // (x -> (+ x 5))
  typeCheck("(x -> (+ x 5))", TyLam(numCon, numCon))

  typeCheck(
    "(x y z -> (+ x 5))",
    TyLam(numCon, TyLam(TyVar("t0"), TyLam(TyVar("t1"), numCon)))
  )

  typeCheck(
    "(x y z -> (== z y))",
    TyLam(TyVar("t0"), TyLam(numCon, TyLam(numCon, boolCon)))
  )

  // TODO: it's pretty obvious that my parser sucks.
  // TODO: i need to allow applying functions to more than one argument.
  typeCheck(
    "(x y z -> (== z (+ x y)))",
    TyLam(numCon, TyLam(numCon, TyLam(numCon, boolCon)))
  )
}