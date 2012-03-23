package typecheck

import scalaz._
import scalaz.ReaderT
import Kleisli._
import scalaz.syntax.monad._
import scalaz.std.either._

object TypeCheckerWithExplicitTypesV2MonadicReader {

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

  val numT = TyCon("Num", Nil)
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

  // promote an either to a Kleisli, ignoring the environment.
  def liftK[T,U](e:Either[String, U])= kleisli[V, T, U]((env: T) => e)

  type V[T] = Either[String, T]

  // TODO: i should be able to import this from somewhere in scalaz
  def local[F[_], A, R](f: (R) => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] =
    Kleisli[F, R, A](r => fa.run(f(r)))

  def typeError(s:String) = Left(s)
  def success(t:Type) = Right(t)

  def find(s:String, env:TypeEnv): Either[String, Type] =
    env.find(_._1 == s).map(p => Right(p._2)).getOrElse(Left("not found: " + s))

  def checkEqual(t1:Type, t2: Type, resultType: Type, errorMessage: String) =
    if(t1 == t2) success(resultType) else typeError(errorMessage)

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp): ReaderT[V, TypeEnv, Type] = expr match {
    case Lit(v) =>liftK(success(litToTy(v)))
    case Id(x) => for (env <- ask[V, TypeEnv]; res <-liftK(find(x, env))) yield res
    case If(tst, texp, fexp) => for {
      // make sure the first branch is a boolean
      t <- typeCheck(tst)
      _ <-liftK(checkEqual(t, boolT, resultType=boolT, "if required bool in test position, but got: " + t))
      // make sure the second and third branches have the same type
      lt <- typeCheck(texp)
      rt <- typeCheck(fexp)
      result <-liftK(checkEqual(lt, rt, resultType = lt, "if branches not the same type, got: " + (lt, rt)))
    } yield result
    case Fun(arg, argType, body) => for {
      t <- local((env:TypeEnv) => env + (arg -> argType))(typeCheck(body))
    } yield TyLam(argType, t)
    case App(operator, operand) => for {
      opType <- typeCheck(operator)
      operandType <- typeCheck(operand)
      res <-liftK(opType match {
        case TyLam(argType, resultType) => checkEqual(argType, operandType, resultType,
          "function expected arg of type: " + argType + ", but got: " + operandType)
        case _ => typeError("function application expected function, but got: " + opType)
      })
    } yield res
  }
}