import TypeChecker._

import org.scalacheck.Prop._
import org.scalacheck.Properties

object TypeCheckerTests extends Properties("TypeChecker"){
  def typeCheck(exp:TFAE, expected:Type, env:TypeEnv = Nil) = {
    val result = realTypeCheckExpr(exp, env)
    property(exp.toString) = secure {
      val result = realTypeCheckExpr(exp, env)
      if(result != expected) println("expected: " + expected + ", got: " + result)
      result == expected
    }
  }

  typeCheck(Num(7), NumT)
  typeCheck(Bool(true), BoolT)
  typeCheck(Bool(false), BoolT)

  typeCheck(Id('x), NumT, List(('x, NumT)))
  //    (test/exn
  //      (real-type-check-expr (id 'x) (list (idtype 'y NumT)))
  //      "type-error unknown id")
  //

  typeCheck(Eql(Num(7), Num(8)), BoolT)
  //    (test/exn (type-check-expr (eql (bool #t)Num(8)))
  //    "eql: type-error expected: NumT in position 1, but found: (boolT)")

  typeCheck(IfThenElse(Bool(true), Num(8), Num(9)), NumT)
  typeCheck(IfThenElse(Bool(false), Num(8), Num(9)), NumT)

  //    (test (type-check-expr (ifthenelse (bool #f) Num(8)Num(9))) NumT)
  //    (test/exn
  //      (type-check-expr (ifthenelse Num(7) Num(8)Num(9)))
  //    "if: type-error expected: (boolT) in position 1, but found: NumT")
  //    (test/exn
  //      (type-check-expr (ifthenelse (bool #t) Num(8)(bool #f)))
  //    "type-error expected same types in 2nd and 3rd positions but got (NumT (boolT))")


  typeCheck(Add(Num(7),Num(7)), NumT)
  typeCheck(Sub(Num(7),Num(7)), NumT)
  //    (test/exn (type-check-expr (add (bool #t) Num(7)))
  //    "add: type-error expected: NumT in position 1, but found: (boolT)")
  //    (test/exn (type-check-expr (sub (bool #t) Num(7)))
  //    "sub: type-error expected: NumT in position 1, but found: (boolT)")
  //    (test/exn (type-check-expr (add Num(7) (bool #t)))
  //    "add: type-error expected: NumT in position 2, but found: (boolT)")
  //    (test/exn (type-check-expr (sub Num(7) (bool #t)))
  //    "sub: type-error expected: NumT in position 2, but found: (boolT)")

  // test functions
  //  [fun (args : (listof symbol)) (ts : (listof Type)) (body : TFAE)]
  typeCheck(
    Fun(List('x -> NumT), Add(Num(5), Num(5))),
    ArrowT(List(NumT), NumT)
  )
  typeCheck(
    Fun(List('x -> NumT), Add(Id('x), Num(5))),
    ArrowT(List(NumT), NumT)
  )
  typeCheck(
    Fun(List('x -> NumT,'y -> NumT,'z -> NumT), Add(Id('x), Num(5))),
    ArrowT(List(NumT, NumT, NumT), NumT)
  )
  typeCheck(
    Fun(List('x -> NumT,'y -> NumT,'z -> NumT), Eql(Id('z), Add(Id('x), Id('y)))),
    ArrowT(List(NumT, NumT, NumT), BoolT)
  )
  typeCheck(
    Fun(List('x -> NumT), Fun(List('y -> BoolT), Num(5))),
    ArrowT(List(NumT), ArrowT(List(BoolT), NumT))
  )
  typeCheck(
    App(Fun(List('x -> NumT), Add(Id('x), Num(5))), List(Num(6))),
    NumT
  )
  typeCheck(
    App(Fun(List('x -> NumT, 'y->BoolT), IfThenElse(Id('y), Add(Id('x), Num(5)), Num(0))), List(Num(7), Bool(false))),
    NumT
  )
  typeCheck(
    App(Fun(List('x -> NumT), Fun(List('y -> BoolT), Add(Id('x), Num(5)))), List(Num(7))),
    ArrowT(List(BoolT), NumT)
  )
  typeCheck(
    App(
      App(Fun(List('x -> NumT), Fun(List('y -> BoolT), Add(Id('x), Num(5)))), List(Num(7))),
      List(Bool(false))
    ),
    NumT
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