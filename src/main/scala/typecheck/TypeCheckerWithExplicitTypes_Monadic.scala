package typecheck

import scalaz.syntax.monad._
import scalaz.std.either._

/**
 *
 */
object TypeCheckerWithExplicitTypes_Monadic {

  trait Literal
  case class Num(i:Int) extends Literal
  case class Bool(b:Boolean) extends Literal

  sealed trait Exp
  case class Id(name:String) extends Exp
  case class Fun(arg: String, argType: Type, body: Exp) extends Exp
  case class App(f:Exp, arg: Exp) extends Exp
  case class Lit(l:Literal) extends Exp
  case class If(tst:Exp, thn:Exp, els:Exp) extends Exp

  trait Type
  case class TyLam(f:Type, arg:Type) extends Type
  case class TyCon(name:String, args: List[Type]) extends Type
  case class TyVar(name:String) extends Type

  type TypeEnv = Map[String, Type]

  val numT  = TyCon("Num", Nil)
  val boolT = TyCon("Bool", Nil)

  def litToTy(l:Literal): Type = l match {
    case Num(_) => numT
    case Bool(_) => boolT
  }

  val predef: TypeEnv = Map(
    "+"   -> (TyLam(numT,  TyLam(numT, numT))),
    "-"   -> (TyLam(numT,  TyLam(numT, numT))),
    "=="  -> (TyLam(numT,  TyLam(numT, boolT))),
    "and" -> (TyLam(boolT, TyLam(boolT, boolT))),
    "or"  -> (TyLam(boolT, TyLam(boolT, boolT)))
    // TODO: how do we do if? is it like this?
    //"if" ->  (TyLam(boolT,  .. ....  Set("a"))
    //  tv => "if" -> (TyLam(boolT, TyLam(tv, TyLam(tv, tv))), Set())
  )

  def find(s:String, env:TypeEnv): Either[String, Type] =
    env.find(_._1 == s).map(p => success(p._2)).getOrElse(typeError("not found: " + s))
  
  def success(t:Type) = Right(t)
  def typeError(msg:String) = Left(msg)

  def compare(t1: Type, t2: Type, resultType: Type, errorMsg: String): Either[String, Type] =
    if(t1 == t2) success(resultType) else typeError(errorMsg)

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp, env: TypeEnv=predef): Either[String, Type] = expr match {
    case Lit(v) => success(litToTy(v))
    case Id(x)  => find(x, env)
    // make sure the first branch is a boolean and then
    // make sure the second and third branches have the same type
    case If(tst, texp, fexp) => for {
      t <- typeCheck(tst, env)
      _ <- compare(t, boolT, boolT, "if required bool in test position, but got: " + t)
      lt <- typeCheck(texp, env)
      rt <- typeCheck(fexp, env)
      result <- compare(lt, rt, lt, "if branches not the same type, got: " + (lt, rt))
    } yield result
    case Fun(arg, argType, body) => for {
      t <- typeCheck(body, env + (arg -> argType))
    } yield TyLam(argType, t)
    case App(operator, operand) => for {
      t <- typeCheck(operator, env)
      res <- t match {
        case TyLam(argType, resultType) => for {
          operandType <- typeCheck(operand, env)
          res2 <- compare(argType, operandType, resultType,
            "function expected arg of type: " + argType + ", but got: " + operandType)
        } yield res2
        case _ => typeError("function application expected function, but got: " + t)
      }
    } yield res
  }
}