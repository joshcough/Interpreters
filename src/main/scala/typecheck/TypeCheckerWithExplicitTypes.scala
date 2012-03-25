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

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp, env: TypeEnv=predef): Type = expr match {
    case Lit(v) => litToTy(v)
    case Id(x) => env.find(_._1 == x).map(_._2).getOrElse(sys.error("not found: " + x))
    case If(tst, texp, fexp) => typeCheck(tst, env) match {
      // make sure the first branch is a boolean
      case t if t == boolT =>
        // make sure the second and third branches have the same type
        // if so, return that type. if not, bomb.
        (typeCheck(texp, env), typeCheck(fexp, env)) match {
          case (lt, rt) if lt == rt => lt
          case (lt, rt) => sys.error("error: if branches not the same type, got: " + (lt, rt))
        }
      case t => sys.error("error: if required bool in test position, but got: " + t)
    }
    case Fun(arg, argType, body) => TyLam(argType, typeCheck(body, env + (arg -> argType)))
    case App(operator, operand) => typeCheck(operator, env) match {
      // make sure the first argument to function application is indeed a function
      case TyLam(argType, resultType) =>
        // then make sure that the arguments match the explicit declarations
        val operandType = typeCheck(operand, env)
        if(argType == operandType) resultType
        else sys.error("function expected arg of type: " + argType + ", but got: " + operandType)
      case t => sys.error("function application expected function, but got: " + t)
    }
  }
}