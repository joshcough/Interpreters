package typecheck

object TypeCheckerWithExplicitTypes_MonadTransformers {

  import TypeCheckerWithExplicitTypesAST._
  import scalaz.ReaderT
  import scalaz.Kleisli
  import scalaz.Kleisli._
  import scalaz.std.either._

  def success(t: Type) = Right(t)
  def typeError(msg: String) = Left(msg)

  def find(s: String, env: TypeEnv): Either[String, Type] =
    env.find(_._1 == s).map(p => success(p._2)).getOrElse(typeError("not found: " + s))

  def compare(t1: Type, t2: Type, resultType: Type, errorMsg: String) =
    if (t1 == t2) success(resultType) else typeError(errorMsg)

  type V[+T] = Either[String, T]

  // liftK essentially promotes an Either to a Kleisli.
  def liftK[T, U](e: Either[String, U]) = kleisli[V, T, U]((env: T) => e)

  // TODO: i should be able to import this from somewhere in scalaz
  def local[F[+_], A, R](f: (R) => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] =
    Kleisli[F, R, A](r => fa.run(f(r)))

  def typeCheck(expr: Exp): ReaderT[V, TypeEnv, Type] = expr match {
    case Lit(v) => liftK(success(litToTy(v)))
    case Id(x)  => for {env <- ask[V, TypeEnv]; res <- liftK(find(x, env))} yield res
    // make sure the first branch is a boolean and then
    // make sure the second and third branches have the same type
    case If(tst, texp, fexp) => for {
      t   <- typeCheck(tst)
      _   <- liftK(compare(t, boolT, boolT, "if required bool in test position, but got: " + t))
      lt  <- typeCheck(texp)
      rt  <- typeCheck(fexp)
      res <- liftK(compare(lt, rt, lt, "if branches not the same type, got: " +(lt, rt)))
    } yield res
    case Fun(arg, argType, body) => for {
      t <- local((env: TypeEnv) => env + (arg -> argType))(typeCheck(body))
    } yield TyLam(argType, t)
    // make sure the first argument to function application is indeed a function
    // then make sure that the arguments match the explicit declarations
    case App(operator, operand) => for {
      operatorType <- typeCheck(operator)
      operandType  <- typeCheck(operand)
      res          <- liftK(operatorType match {
        case TyLam(argType, resultType) =>
          compare(argType, operandType, resultType,
            "function expected arg of type: " + argType + ", but got: " + operandType)
        case _ => typeError("function application expected function, but got: " + operatorType)
      })
    } yield res
  }
}