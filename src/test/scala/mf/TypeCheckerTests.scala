package mf

import AST._
import scalaz.std.either._
import scalaz.syntax.monad._

// TODO: make these tests run against all the type checkers, where possible.
object TypeCheckerTests extends org.scalacheck.Properties("TypeChecker") with util.Compare {

  typeCheckExp("7", "Int")
  typeCheckExp("true", "Bool")
  typeCheckExp("false", "Bool")
  typeCheckExp("(id 7)", "Int")
  typeCheckExp("(== 7 8)", "Bool")
  typeCheckExp("(+ 7 8)", "Int")
  typeCheckExp("(- 7 8)", "Int")

  // test functions
  typeCheckExp("(x -> (+ 5 5))", "'t0 -> Int")
  typeCheckExp("(x -> (+ x 5))", "Int -> Int")
  typeCheckExp("(x y z -> (+ x 5))", "Int -> 't0 -> 't1 -> Int")
  typeCheckExp("(x y z -> (== z y))", "'t0 -> Int -> Int -> Bool")
  typeCheckExp("(x y z -> (== z (+ x y)))", "Int -> Int -> Int -> Bool")

  // f = t0 -> t1, g = t1 -> t2, x = t0
  typeCheckExp("(f g x -> (g (f x)))", "('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2")
  typeCheckExp("(if true 5 6)", "Int")
  typeCheckExp("if", "Bool -> 't0 -> 't0 -> 't0")
  typeCheckExp("((id if) (id true) (id 5) (id 6))", "Int")


  typeCheck("(def id (x -> x)) (val res 7)", Map("id" -> "'t0 -> 't0", "res" -> "Int"))


//  typeCheckExp("(Cons 5 (Cons 6 Nil))", "List Int")
//  typeCheckExp("Nil", "List 't0")

// TODO: add tests for failures. here is one example:
//  typeCheckFailure("(if true 5 true)", "Unable to unify (Int, Bool)")

  def typeCheckExp(code: String, expectedType:String) = {
    val a = TyVar("a")
    val predef: Env = Map(
      Name("id")  -> TyForall(List(a), TyLam(a, a)),
      Name("+")   -> TyLam(IntT, TyLam(IntT, IntT)),
      Name("-")   -> TyLam(IntT, TyLam(IntT, IntT)),
      Name("==")  -> TyLam(IntT, TyLam(IntT, BoolT)),
      Name("and") -> TyLam(BoolT, TyLam(BoolT, BoolT)),
      Name("or")  -> TyLam(BoolT, TyLam(BoolT, BoolT)),
      Name("if")  -> TyForall(List(a), TyLam(BoolT, TyLam(a, TyLam(a, a)))),
      Name("Nil") -> listCon(a),
      Name("Cons")-> TyForall(List(a), TyLam(a, TyLam(listCon(a), listCon(a))))
    )
    compare (code, TypeChecker.typeCheckExp(code, predef), Parser.parseType(expectedType))
  }
  def typeCheck(code: String, expectedTypes:Map[String, String]) = {
    compareR (
      code,
      for{ tcp <- TypeChecker.typeCheck(code) } yield tcp.types.map{ case (n,t) => (n.name, t.toString)},
      expectedTypes.map{ case (k,v) => (k, Parser.parseType(v).right.get)}
    )
  }
}
