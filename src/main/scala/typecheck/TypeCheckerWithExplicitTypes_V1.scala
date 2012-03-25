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
object TypeCheckerWithExplicitTypes_V1 {

  sealed trait Type
  case object NumT extends Type
  case object BoolT extends Type
  case class ArrowT(args:List[Type], result:Type) extends Type

  sealed trait Tree
  case class Num(n:Int) extends Tree
  case class Bool(b:Boolean) extends Tree
  case class Add(l:Tree, r:Tree) extends Tree
  case class Sub(l:Tree, r:Tree) extends Tree
  case class Eql(l:Tree, r:Tree) extends Tree
  case class Id(name:Symbol) extends Tree
  case class If(tst:Tree, thn:Tree, els:Tree) extends Tree
  case class Fun(formals:List[(Symbol, Type)], body: Tree) extends Tree
  case class App(operator:Tree, operands:List[Tree]) extends Tree

  type TypeEnv = List[(Symbol, Type)]

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Tree, env: TypeEnv=Nil): Type = expr match {
    case Num(n) => NumT
    case Bool(b) => BoolT
    case Id(x) => env.find(_._1 == x).map(_._2).getOrElse(sys.error("not found: " + x))
    case Add(l, r) => mathTypeCheck(l, r, env, NumT, "add")
    case Sub(l, r) => mathTypeCheck(l, r, env, NumT, "sub")
    case Eql(l, r) => mathTypeCheck(l, r, env, BoolT, "add")
    case If(tst, texp, fexp) => typeCheck(tst, env) match {
      // make sure the first branch is a BoolT
      case BoolT =>
        // make sure the second and third branches have the same type
        // if so, return that type. if not, bomb.
        (typeCheck(texp, env), typeCheck(fexp, env)) match {
          case (lt, rt) if lt == rt => lt
          case (lt, rt) => sys.error("error: if branches not the same type, got: " + (lt, rt))
        }
      case t => sys.error("error: ifthenelse required bool in test position, but got: " + t)
    }
    case Fun(formals, body) => ArrowT(formals.map(_._2), typeCheck(body, formals ++ env))
    case App(operator, operands) => typeCheck(operator, env) match {
      // make sure the first argument to function application is indeed a function
      case ArrowT(argTypes, resultType) =>
        // then make sure that the arguments match the explicit declarations
        if(argTypes.zip(operands.map(typeCheck(_, env))).forall(tt => tt._1 == tt._2)) resultType
        else sys.error("function expected args of type: " + argTypes.mkString(", ") + ", but got: " + operands.mkString(", "))
      case t => sys.error("function application expected function, but got: " + t)
    }
  }

  /**
   * Make sure l and r are both IntT, and if so, return the given returnType
   * (IntT for add and sub, BoolT for eql). If not, bomb.
   */
  def mathTypeCheck(l: Tree, r: Tree, env: TypeEnv, returnType: Type, function: String): Type = {
    (typeCheck(l, env), typeCheck(r, env)) match {
      case (lt, rt) if lt == rt => returnType
      case (lt, rt) => sys.error("error: " + function + " expected two NumT arguments, but got: " + (lt, rt))
    }
  }
}