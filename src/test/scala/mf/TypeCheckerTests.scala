package mf

import AST._
import scalaz.std.either._
import scalaz.syntax.monad._
import TypeChecker._

object TypeCheckExpressionTests extends org.scalacheck.Properties("TypeChecker for Expressions") with util.Compare {
  typeCheckExp("7", "Int")
  typeCheckExp("True", "Bool")
  typeCheckExp("False", "Bool")
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
  typeCheckExp("(if True 5 6)", "Int")
//  typeCheckExp("if", "Bool -> 't0 -> 't0 -> 't0")
  typeCheckExp("((id if) (id True) (id 5) (id 6))", "Int")

  typeCheckExp("(f x -> (id x))", "'t0 -> 't1 -> 't1")
  typeCheckExp("(f x -> (f (id x)))", "('t0 -> 't1) -> 't0 -> 't1")


  def typeCheckExp(code: String, expectedType:String) = {
    /**
     * This stuff uses a hard coded Bool, even though I can represent Bool
     * as an algebraic data type now. But this is fine for testing single expressions,
     * it's kind of like assuming that Bool has been loaded.
     */
    val a = TyVar("a")
    val BoolT = TyCon(Name("Bool"), Nil)
    val predef: TypeEnv = Map(
      Name("id")    -> TyForall(List(a), TyLam(a, a)),
      Name("+")     -> TyLam(IntT, TyLam(IntT, IntT)),
      Name("-")     -> TyLam(IntT, TyLam(IntT, IntT)),
      Name("==")    -> TyLam(IntT, TyLam(IntT, BoolT)),
      Name("and")   -> TyLam(BoolT, TyLam(BoolT, BoolT)),
      Name("or")    -> TyLam(BoolT, TyLam(BoolT, BoolT)),
      Name("if")    -> TyForall(List(a), TyLam(BoolT, TyLam(a, TyLam(a, a)))),
      Name("True")  -> BoolT,
      Name("False") -> BoolT
    )
    compare (code,
    {
      val res = TypeChecker.typeCheckExp(code, predef)
      //println(getTVarsOfType(res.right.get))
      res
    }, Parser.parseType(expectedType))
  }
}

object TypeCheckerTests extends org.scalacheck.Properties("TypeChecker") with util.Compare {


  typeCheck("(def id (x -> x)) (val res 7)", Map("id" -> "'t0 -> 't0", "res" -> "Int"))
  typeCheck("(data List ('a) ((Nil) (Cons 'a (List 'a)))) (val x (Cons 7 Nil))", Map("x" -> "List Int"))

  // TODO: look into keeping the same type names here, instead of moving to 't0 etc
  typeCheck("(data List ('a) ((Nil) (Cons 'a (List 'a)))) (val x Nil)", Map("x" -> "List 't0"))

  typeCheck("""
(data Bool () ((True) (False)))
(data List ('a) ((Nil) (Cons 'a (List 'a))))
(data Maybe ('a) ((Nothing) (Just 'a)))

(def id (x -> x))
(val t (id True))
(val l (Cons 5 (Cons 6 Nil)))
(val m (Just (id l)))

""", Map(
    "id" -> "'t0 -> 't0",
    "t" -> "Bool",
    "l" -> "List Int",
    "m" -> "Maybe (List Int)"))

  //  TODO: add tests for failures. here is one example:
  //  typeCheckFailure("(if True 5 True)", "Unable to unify (Int, Bool)")

  def typeCheck(code: String, expectedTypes:Map[String, String]) = compareR (
    code,
    for{ env <- TypeChecker.typeCheck(code) } yield env.map{ case (n,t) => (n.name, t.toString)},
    expectedTypes.map{ case (k,v) => (k, Parser.parseType(v).right.get)}
  )
}

/**
Notes for later...
typeCheckExp("(f x -> (id x))", "'t0 -> 't1 -> 't1")
typeCheckExp("(f x -> (f (id x)))", "('t0 -> 't1) -> 't0 -> 't1")

(def id (x -> x)) = 't0 -> 't0 => forall 't0. 't0 -> 't0
(def foo (f x -> (f (id x)))) = ('t0 -> 't1) -> 't0 -> 't1 => forall 't0 't1. ('t0 -> 't1) -> 't0 -> 't1

**/