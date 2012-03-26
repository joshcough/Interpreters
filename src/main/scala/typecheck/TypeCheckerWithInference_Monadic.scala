package typecheck

import scalaz.State
import scalaz.State._
import scalaz.std.list.{listInstance => listTraverse}

/**
 * Note: A lot of this code is a port from:
 *   http://fsharpcode.blogspot.com/2010/08/hindley-milner-type-inference-sample.html
 *
 * TODO: comment is out of date.
 *
 * I'm writing this to explore type checking with type inference in Scala.
 *
 * To do this, I'm starting with a very simple language that allows
 * only one top level expression.
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
 */

/**
 *  TODO: these add let...
 * What happens here?
 * let id x = x in if (id true) (id 6) (id 7)
 */

object TypeCheckerWithInference_Monadic {

  // AST

  trait Literal
  case class Num(i: Int) extends Literal
  case class Bool(b: Boolean) extends Literal

  trait Exp
  case class Var(name: String) extends Exp
  case class Lam(arg: String, body: Exp) extends Exp
  case class App(f: Exp, arg: Exp) extends Exp
  case class Lit(l: Literal) extends Exp
  //  Let      of string * Exp * Exp   // local definition

  // Type Tree

  trait Type
  case class TyLam(f: Type, arg: Type) extends Type
  case class TyVar(name: String) extends Type
  case class TyCon(name: String, args: List[Type]) extends Type

  // Subtitutions

  type Subst = Map[String, Type]
  def extend(v: String, t: Type, subs: Subst): Subst = subs + (v -> t)
  def lookup(v: String, subs: Subst): Type = subs.getOrElse(v, TyVar(v))
  def subs(t: Type, s: Subst): Type = t match {
    case TyVar(n) =>
      val tp = lookup(n, s)
      if (t == tp) tp else subs(tp, s)
    case TyLam(a, r) => TyLam(subs(a, s), subs(r, s))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(subs(_, s)))
  }

  // Environments

  type TyScheme = (Type, Set[String])
  type Env = Map[String, TyScheme]
  // TODO: TyScheme is used for Let, which I haven't done yet.
  // This implementation could just have type Env = Map[String, Type]
  def getTVarsOfType(t: Type): Set[String] = t match {
    case TyVar(n) => Set(n)
    case TyLam(t1, t2) => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args) => args.flatMap(t => getTVarsOfType(t)).toSet
  }

  // Unification

  // Calculate the most general unifier of two types,
  // raising a UnificationError if there isn't one
  def mgu(a: Type, b: Type, s: Subst): Subst = {
    //println("calculating mgu for: " + (a, b, s))
    val result = (subs(a, s), subs(b, s)) match {
      case (TyVar(ta), TyVar(tb)) if ta == tb => s
      // this does the 'occurs' check for infinite types.
      case (TyVar(ta), _) if (!getTVarsOfType(b).contains(ta)) =>
        extend(ta, b, s)
      case (_, TyVar(_)) => mgu(b, a, s)
      case (TyLam(a1, b1), TyLam(a2, b2)) => mgu(a1, a2, mgu(b1, b2, s))
      case (TyCon(name1, args1), TyCon(name2, args2)) if name1 == name2 =>
        args1.zip(args2).foldLeft(s) {
          case (sp, (t1, t2)) => mgu(t1, t2, sp)
        }
      case (x, y) => sys.error("unable to unify: " +(x, y))
    }
    //println("mgu = " + result)
    result
  }

  // Calculate the principal type scheme for an expression in a given
  // typing environment
  def tp(env: Env, exp: Exp, bt: Type, s: Subst): State[Int, Subst] = {
    def newTypVar = for (n <- modify[Int](_ + 1)) yield TyVar("t" + n)
    exp match {
      case Lit(v) => state(mgu(litToTy(v), bt, s))
      case Var(n) => state(env.get(n).map {
        case (t, _) => mgu(subs(t, s), bt, s)
      }.getOrElse(sys.error("unknown id: " + n)))
      case Lam(x, e) => for {
        a <- newTypVar
        b <- newTypVar
        t <- tp(env + (x ->(a, Set())), e, b, mgu(bt, TyLam(a, b), s))
      } yield t
      case App(e1, e2) => for {
        a <- newTypVar
        funT <- tp(env, e1, TyLam(a, bt), s)
        resT <- tp(env, e2, a, funT)
      } yield resT
    }
  }

  val numCon = TyCon("Num", Nil)
  val boolCon = TyCon("Bool", Nil)

  def litToTy(l: Literal): Type = l match {
    case Num(_) => numCon
    case Bool(_) => boolCon
  }

  val predef: Env = Map(
    "+" ->(TyLam(numCon, TyLam(numCon, numCon)), Set()),
    "-" ->(TyLam(numCon, TyLam(numCon, numCon)), Set()),
    "==" ->(TyLam(numCon, TyLam(numCon, boolCon)), Set()),
    "and" ->(TyLam(boolCon, TyLam(boolCon, boolCon)), Set()),
    "or" ->(TyLam(boolCon, TyLam(boolCon, boolCon)), Set())
    // TODO: how do we do if? is it like this?
    //  tv => "if" -> (TyLam(boolCon, TyLam(tv, TyLam(tv, tv))), Set())
  )

  // rename all the type variables starting from t0
  // this cleans things up a bit as far as presentation goes.
  def renameTyVars(t: Type): Type = {
    type R = (Int, Map[String, String])
    def update(k: String, r: R) =
      if (r._2.contains(k)) r else (r._1 + 1, r._2 + (k -> ("t" + r._1)))
    def helper(t: Type): State[R, Type] = t match {
      case TyVar(oldName) =>
        for {z <- modify[R](update(oldName, _))} yield TyVar(z._2(oldName))
      case TyLam(a, b) =>
        for {t1 <- helper(a); t2 <- helper(b)} yield TyLam(t1, t2)
      case TyCon(name, tyArgs) =>
        for {ts <- listTraverse.traverseS(tyArgs)(helper(_))} yield TyCon(name, ts)
    }
    helper(t)((0, Map[String, String]()))._1
  }

  // the top level type check function
  def typeCheck(exp: Exp): Type = {
    val a = TyVar("init")
    renameTyVars(subs(a, tp(predef, exp, a, Map())(0)._1))
  }
}
