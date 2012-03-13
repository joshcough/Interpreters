
object TypeChecker {

  sealed trait Type
  case object NumT extends Type
  case object BoolT extends Type
  case class ArrowT(args:List[Type], result:Type) extends Type

  sealed trait TFAE
  case class Num(n:Int) extends TFAE
  case class Bool(b:Boolean) extends TFAE
  case class Add(l:TFAE, r:TFAE) extends TFAE
  case class Sub(l:TFAE, r:TFAE) extends TFAE
  case class Eql(l:TFAE, r:TFAE) extends TFAE
  case class Id(name:Symbol) extends TFAE
  case class IfThenElse(tst:TFAE, thn:TFAE, els:TFAE) extends TFAE
  type Formal = (Symbol, Type)
  case class Fun(formals:List[Formal], body: TFAE) extends TFAE
  case class App(operator:TFAE, operands:List[TFAE]) extends TFAE

  type Typed = (Symbol, Type)
  type TypeEnv = List[Typed]

  def typeCheckExpr(expr: TFAE): Type = realTypeCheckExpr(expr, Nil)
  def realTypeCheckExpr(expr: TFAE, env: TypeEnv): Type = expr match {
    case Num(n) => NumT
    case Bool(b) => BoolT
    case Id(x) => env.find(_._1 == x).map(_._2).getOrElse(sys.error("not found: " + x))
    case Add(l, r) => mathTypeCheck(l, r, env, NumT, "add")
    case Sub(l, r) => mathTypeCheck(l, r, env, NumT, "sub")
    case Eql(l, r) => mathTypeCheck(l, r, env, BoolT, "add")
    case IfThenElse(tst, texp, fexp) => realTypeCheckExpr(tst, env) match {
      case BoolT =>
        val lType = realTypeCheckExpr(texp, env)
        val rType = realTypeCheckExpr(fexp, env)
        if(lType == rType) lType
        else sys.error("error: if branches not the same type, got: " + (lType, rType))
      case t => sys.error("error: ifthenelse required bool in test position, but got: " + t)
    }
    case Fun(formals, body) => ArrowT(formals.map(_._2), realTypeCheckExpr(body, formals ++ env))
    case App(operator, operands) => realTypeCheckExpr(operator, env) match {
      case ArrowT(argTypes, resultType) =>
        if(argTypes.zip(operands.map(realTypeCheckExpr(_, env))).forall(tt => tt._1 == tt._2)) resultType
        else sys.error("function expected args of type: " + argTypes.mkString(", ") + ", but got: " + operands.mkString(", "))
      case t => sys.error("function application expected function, but got: " + t)
    }
  }

  def mathTypeCheck(l : TFAE, r : TFAE, env : TypeEnv, returnType: Type, function: String): Type = {
    val lType = realTypeCheckExpr(l, env)
    val rType = realTypeCheckExpr(r, env)
    if(lType == NumT && rType == NumT) returnType
    else sys.error("error: " + function + " expected two NumT arguments, but got: " + (lType, rType))
  }
  
  def main(args:Array[String]) {
    
    def check(exp:TFAE, expected:Type, env:TypeEnv = Nil) = {
      val result = realTypeCheckExpr(exp, env)
      assert(realTypeCheckExpr(exp, env) == expected, "expected: " + expected + ", got: " + result)
    }

    check(Num(7), NumT)
    check(Bool(true), BoolT)
    check(Bool(false), BoolT)

    check(Id('x), NumT, List(('x, NumT)))
//    (test/exn
//      (real-type-check-expr (id 'x) (list (idtype 'y NumT)))
//      "type-error unknown id")
//

    check(Eql(Num(7), Num(8)), BoolT)
//    (test/exn (type-check-expr (eql (bool #t)Num(8)))
//    "eql: type-error expected: NumT in position 1, but found: (boolT)")

    check(IfThenElse(Bool(true), Num(8), Num(9)), NumT)
    check(IfThenElse(Bool(false), Num(8), Num(9)), NumT)

//    (test (type-check-expr (ifthenelse (bool #f) Num(8)Num(9))) NumT)
//    (test/exn
//      (type-check-expr (ifthenelse Num(7) Num(8)Num(9)))
//    "if: type-error expected: (boolT) in position 1, but found: NumT")
//    (test/exn
//      (type-check-expr (ifthenelse (bool #t) Num(8)(bool #f)))
//    "type-error expected same types in 2nd and 3rd positions but got (NumT (boolT))")


    check(Add(Num(7),Num(7)), NumT)
    check(Sub(Num(7),Num(7)), NumT)
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
    check(
      Fun(List('x -> NumT), Add(Num(5), Num(5))),
      ArrowT(List(NumT), NumT)
    )
    check(
      Fun(List('x -> NumT), Add(Id('x), Num(5))),
      ArrowT(List(NumT), NumT)
    )
    check(
      Fun(List('x -> NumT,'y -> NumT,'z -> NumT), Add(Id('x), Num(5))),
      ArrowT(List(NumT, NumT, NumT), NumT)
    )
    check(
      Fun(List('x -> NumT,'y -> NumT,'z -> NumT), Eql(Id('z), Add(Id('x), Id('y)))),
      ArrowT(List(NumT, NumT, NumT), BoolT)
    )
    check(
      Fun(List('x -> NumT), Fun(List('y -> BoolT), Num(5))),
      ArrowT(List(NumT), ArrowT(List(BoolT), NumT))
    )
    check(
      App(Fun(List('x -> NumT), Add(Id('x), Num(5))), List(Num(6))),
      NumT
    )
    check(
      App(Fun(List('x -> NumT, 'y->BoolT), IfThenElse(Id('y), Add(Id('x), Num(5)), Num(0))), List(Num(7), Bool(false))),
      NumT
    )
    check(
      App(Fun(List('x -> NumT), Fun(List('y -> BoolT), Add(Id('x), Num(5)))), List(Num(7))),
      ArrowT(List(BoolT), NumT)
    )
    check(
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
}