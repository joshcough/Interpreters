package typecheck

object TypeCheckerWithExplicitTypesAST {

  trait Literal
  case class Num(i: Int) extends Literal
  case class Bool(b: Boolean) extends Literal

  sealed trait Exp
  case class Id(name: String) extends Exp
  case class Fun(arg: String, argType: Type, body: Exp) extends Exp
  case class App(f: Exp, arg: Exp) extends Exp
  case class Lit(l: Literal) extends Exp
  case class If(tst: Exp, thn: Exp, els: Exp) extends Exp

  trait Type
  case class TyLam(f: Type, arg: Type) extends Type
  case class TyCon(name: String, args: List[Type]) extends Type
  case class TyVar(name: String) extends Type

  type TypeEnv = Map[String, Type]

  val numT = TyCon("Num", Nil)
  val boolT = TyCon("Bool", Nil)

  def litToTy(l: Literal): Type = l match {
    case Num(_) => numT
    case Bool(_) => boolT
  }

  val predef: TypeEnv = Map(
    "+" -> (TyLam(numT, TyLam(numT, numT))),
    "-" -> (TyLam(numT, TyLam(numT, numT))),
    "==" -> (TyLam(numT, TyLam(numT, boolT))),
    "and" -> (TyLam(boolT, TyLam(boolT, boolT))),
    "or" -> (TyLam(boolT, TyLam(boolT, boolT)))
    // TODO: how do we do if? is it like this?
    //"if" ->  (TyLam(boolT,  .. ....  Set("a"))
    //  tv => "if" -> (TyLam(boolT, TyLam(tv, TyLam(tv, tv))), Set())
  )
}
