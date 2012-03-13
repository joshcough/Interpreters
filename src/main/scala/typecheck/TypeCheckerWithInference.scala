package typecheck

/**
 * I'm writing this to explore type checking with type inference in Scala.
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
 * The user is not required to put explicit type declarations on function arguments,
 * or the function return type. These types will be inferred by the type checker.
 *
 * Functions in this language cannot be curried, they must be applied to all of their arguments.
 *
 * There is no concrete syntax for the language, only abstract. But, you easily imagine possible examples:
 *
 * Here, we create a function that takes two Ints and simply adds them.
 * We then apply that function to 7 and 8.
 *
 * ((fun (x, y) (add x y)) 7 8)
 *
 * Here is an example of 'not': (fun (x) (if x false true))
 */
object TypeCheckerWithInference {

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
  case class IfThenElse(tst:Tree, thn:Tree, els:Tree) extends Tree
  case class Fun(formals:Symbol, body: Tree) extends Tree
  case class App(operator:Tree, operands:List[Tree]) extends Tree

}