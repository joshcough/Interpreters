package typecheck

/**
 * This language requires the user to put explicit type declarations on
 * all function arguments.
 *
 * It has three types: Int, Bool, and Arrow (List T) T
 *
 * There are also a few built-in functions: add, sub, and eql, which take two int arguments.
 *
 * The type checker here pretty much just makes sure that the actual types line up
 * with the annotations. This is nothing fancy, but it gives us a framework for starting
 * to explore type inference.
 */
object TypeCheckerWithExplicitTypes {

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
}