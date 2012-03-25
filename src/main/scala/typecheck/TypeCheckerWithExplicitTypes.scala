package typecheck

/**
 * I'm writing this to explore type checking in Scala.
 *
 * To do this, I'm starting with a very simple language that allows only one top level expression.
 *
 * The language has three types: Int, Bool, and Arrow (List T) T
 * and corresponding literals for those types (shown in examples below).
 *
 * There are four built-in functions:
 *   if  : (Bool -> T -> T) -> T
 *   add : (Int  -> Int) -> Int
 *   sub : (Int  -> Int) -> Int
 *   eql : (Int  -> Int) -> Bool
 *
 * The user is required to put explicit type declarations on all function arguments.
 * It does not however, require that the function return type be annotated.
 * This *could* be required - and it would require some additional
 * type checking for functions. But in this case, it simply isn't necessary.
 * The return type of a function is simply the type of its body (as long as
 * the body type checks properly, of course).
 *
 * Functions in this language cannot be curried, they must be applied to all of their arguments.
 *
 * There is no concrete syntax for the language, only abstract. But, you easily imagine possible examples:
 *
 * Here, we create a function that takes two Ints and simply adds them.
 * We then apply that function to 7 and 8.
 *
 * ((fun (x:Int, y:Int) (add x y)) 7 8)
 *
 * Here is an example of 'not': (fun (x: Bool) (if x false true))
 *
 * The type checker pretty much just makes sure that the actual types line up
 * with the for the built-ins, and that the arguments passed to a function line up with
 * the explicit function argument annotations.
 *
 * So far, this is nothing fancy. But it gives us a framework for starting to explore other features like:
 *   type inference
 *   partial application
 *   new data types
 *   higher kinded types
 *   type classes
 *
 * We also have the opportunity later to refactor the type checker to a monadic style in
 * an effort to explore monads and monad transformers.
 */
object TypeCheckerWithExplicitTypes {

  import TypeCheckerWithExplicitTypesAST._

  def success(t:Type) = t
  def typeError(msg:String) = sys.error(msg)

  def find(s:String, env:TypeEnv): Type =
    env.find(_._1 == s).map(p => success(p._2)).getOrElse(sys.error("not found: " + s))

  def compare(t1: Type, t2: Type, resultType: Type, errorMsg: String): Type =
    if(t1 == t2) success(resultType) else typeError(errorMsg)

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp, env: TypeEnv=predef): Type = expr match {
    case Lit(v) => success(litToTy(v))
    case Id(x)  => find(x, env)
    // make sure the first branch is a boolean and then
    // make sure the second and third branches have the same type
    case If(tst, texp, fexp) =>
      val t  = typeCheck(tst, env)
      val _ = compare(t, boolT, boolT, "error: if required bool in test position, but got: " + t)
      val lt = typeCheck(texp, env)
      val rt = typeCheck(fexp, env)
      val res = compare(lt, rt, lt, "error: if branches not the same type, got: " + (lt, rt))
      res
    case Fun(arg, argType, body) =>
      val t = typeCheck(body, env + (arg -> argType))
      TyLam(argType, t)
    // make sure the first argument to function application is indeed a function
    // then make sure that the arguments match the explicit declarations
    case App(operator, operand) =>
      val operatorType = typeCheck(operator, env)
      val operandType  = typeCheck(operand,  env)
      val res = operatorType match {
        case TyLam(argType, resultType) =>
          compare(argType, operandType, resultType,
            "function expected arg of type: " + argType + ", but got: " + operandType)
        case t => typeError("function application expected function, but got: " + t)
      }
      res
  }
}