package typecheck

import typecheck.TypeCheckerWithInferenceAST._

// TODO: make these tests run against all the type checkers, where possible.
object TypeCheckerWithInferenceTests extends 
  org.scalacheck.Properties("TypeCheckerWithInference") with util.Compare {

  typeCheck(Lit(Num(7)), IntT)
  typeCheck(Lit(Bool(true)), BoolT)
  typeCheck(Lit(Bool(false)), BoolT)

//  typeCheck(Id('x), NumT, List((Id('x), NumT)))
//    (test/exn
//      (real-type-check-expr (id 'x) (list (idtype 'y NumT)))
//      "type-error unknown id")
//

    typeCheck(App(Id("identity"), Lit(Num(7))), IntT)
    typeCheck(App(App(Id("=="), Lit(Num(7))), Lit(Num(8))), BoolT)
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

  typeCheck(App(App(Id("+"), Lit(Num(7))), Lit(Num(8))), IntT)
  typeCheck(App(App(Id("-"), Lit(Num(7))), Lit(Num(8))), IntT)

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
    Lam(Id("f"),
      Lam(Id("g"),
        Lam (Id("x"),
          App(Id("g"), App(Id("f"), Id("x"))))))

  typeCheck(composeAst,
    TyLam(
      TyLam(TyVar("t0"),TyVar("t1")),
        TyLam(TyLam(TyVar("t1"),TyVar("t2")),
          TyLam(TyVar("t0"),TyVar("t2")))))

  // (x -> (+ 5 5))
  typeCheck(
    Lam(Id("x"), App(App(Id("+"), Lit(Num(5))), Lit(Num(5)))),
    TyLam(TyVar("t0"), IntT)
  )
  typeCheck(App(App(Id("+"), Lit(Num(5))), Lit(Num(5))), IntT)

  // (x -> (+ x 5))
  typeCheck(
    Lam(Id("x"), App(App(Id("+"), Id("x")), Lit(Num(5)))),
    TyLam(IntT, IntT)
  )

  // (x y z -> (+ x 5))
  typeCheck(
    Lam(Id("x"), Lam(Id("y"), Lam(Id("z"), App(App(Id("+"), Id("x")), Lit(Num(5)))))),
    TyLam(IntT, TyLam(TyVar("t0"), TyLam(TyVar("t1"), IntT)))
  )

  // (x y z -> (== z (+ z y)))
  typeCheck(
    Lam(Id("x"), Lam(Id("y"), Lam(Id("z"), App(App(Id("=="), Id("z")), App(App(Id("+"), Id("x")), Id("y")))))),
    TyLam(IntT, TyLam(IntT, TyLam(IntT, BoolT)))
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

  def typeCheck(exp:Exp, expected:Type) =
    compare(exp.toString,  TypeChecker.typeCheck(exp), Right(expected))
}