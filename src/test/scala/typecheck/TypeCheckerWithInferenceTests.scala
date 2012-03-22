package typecheck

import TypeCheckerWithInference._

import org.scalacheck.Prop._
import org.scalacheck.Properties

object TypeCheckerWithInferenceTests extends Properties("TypeCheckerWithExplicitTypes"){

  def typeCheck(exp:Exp, expected:Type) = {
    property(exp.toString) = secure {
      val result = TypeCheckerWithInference.typeCheck(exp)
      if(result != expected) println("expected: " + expected + ", got: " + result)
      result == expected
    }
  }

  typeCheck(Lit(Num(7)), numCon)
  typeCheck(Lit(Bool(true)), boolCon)
  typeCheck(Lit(Bool(false)), boolCon)

//  typeCheck(Var('x), NumT, List((Var('x), NumT)))
//    (test/exn
//      (real-type-check-expr (id 'x) (list (idtype 'y NumT)))
//      "type-error unknown id")
//

  typeCheck(App(App(Var("=="), Lit(Num(7))), Lit(Num(8))), boolCon)
//    (test/exn (type-check-expr (eql (bool #t)Num(8)))
//    "eql: type-error expected: NumT in position 1, but found: (boolT)")

//  typeCheck(If(Bool(true), Num(8), Num(9)), NumT)
//  typeCheck(If(Bool(false), Num(8), Num(9)), NumT)

//    (test (type-check-expr (ifthenelse (bool #f) Num(8)Num(9))) NumT)
//    (test/exn
//      (type-check-expr (ifthenelse Num(7) Num(8)Num(9)))
//    "if: type-error expected: (boolT) in position 1, but found: NumT")
//    (test/exn
//      (type-check-expr (ifthenelse (bool #t) Num(8)(bool #f)))
//    "type-error expected same types in 2nd and 3rd positions but got (NumT (boolT))")

  typeCheck(App(App(Var("+"), Lit(Num(7))), Lit(Num(8))), numCon)
  typeCheck(App(App(Var("-"), Lit(Num(7))), Lit(Num(8))), numCon)

//    "add: type-error expected: NumT in position 1, but found: (boolT)")
//    (test/exn (type-check-expr (sub (bool #t) Num(7)))
//    "sub: type-error expected: NumT in position 1, but found: (boolT)")
//    (test/exn (type-check-expr (add Num(7) (bool #t)))
//    "add: type-error expected: NumT in position 2, but found: (boolT)")
//    (test/exn (type-check-expr (sub Num(7) (bool #t)))
//    "sub: type-error expected: NumT in position 2, but found: (boolT)")

  // test functions

  // (f g -> (g (f x))
  // f = t0 -> t1, g = t1 -> t2, x = t0
  val composeAst =
    Lam("f",
      Lam("g",
        Lam ("x",
          App(Var("g"), App(Var("f"), Var ("x"))))))

  typeCheck(composeAst,
    TyLam(
      TyLam(TyVar("t0"),TyVar("t1")),
        TyLam(TyLam(TyVar("t1"),TyVar("t2")),
          TyLam(TyVar("t0"),TyVar("t2")))))

  // (x -> (+ 5 5))
  typeCheck(
    Lam("x", App(App(Var("+"), Lit(Num(5))), Lit(Num(5)))),
    TyLam(TyVar("t0"), numCon)
  )
  typeCheck(App(App(Var("+"), Lit(Num(5))), Lit(Num(5))), numCon)

  // (x -> (+ x 5))
  typeCheck(
    Lam("x", App(App(Var("+"), Var("x")), Lit(Num(5)))),
    TyLam(numCon, numCon)
  )

  // (x y z -> (+ x 5))
  typeCheck(
    Lam("x", Lam("y", Lam("z", App(App(Var("+"), Var("x")), Lit(Num(5)))))),
    TyLam(numCon, TyLam(TyVar("t0"), TyLam(TyVar("t1"), numCon)))
  )

  // (x y z -> (== z (+ z y)))
  typeCheck(
    Lam("x", Lam("y", Lam("z", App(App(Var("=="), Var("z")), App(App(Var("+"), Var("x")), Var("y")))))),
    TyLam(numCon, TyLam(numCon, TyLam(numCon, boolCon)))
  )

  /*
(test/exn
  (type-check-expr (app Num(8) (list Num(8))))
  "app: type-error expected: function in position 1, but found: NumT")

(test/exn
  (type-check-expr
   ; pass a bool to a function wanting a num
   (app (fun (list 'x) (list NumT) (add (id 'x)Num(5))) (list (bool #t))))
  "app: type-error expected: (NumT) but found: ((boolT))")

(test/exn
  (type-check-expr
   (app (fun (list 'x 'y) (list NumT (boolT)) (id 'x)) (list (bool #t))))
  "app: type-error expected: (NumT (boolT)) but found: ((boolT))")

(test/exn
  (type-check-expr
   (app (fun (list 'x 'y) (list NumT (boolT)) (id 'x)) (list Num(7) Num(7))))
  "app: type-error expected: (NumT (boolT)) but found: (NumT NumT)")

(test/exn
  (type-check-expr
   (app (fun (list 'x 'y) (list NumT (boolT)) (id 'x)) (list (bool #t) (bool #t))))
  "app: type-error expected: (NumT (boolT)) but found: ((boolT) (boolT))")

; other tests
(test/exn
  (type-check-expr (ifthenelse (eql (bool #t)Num(8)) Num(8)Num(9)))
  "eql: type-error expected: NumT in position 1, but found: (boolT)")
  */
}