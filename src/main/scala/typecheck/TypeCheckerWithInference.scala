package typecheck

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

object TypeCheckerWithInference {

  import TypeCheckerWithInferenceAST._

  // Unification

  // Calculate the most general unifier of two types,
  // raising a UnificationError if there isn't one
  def mgu(a:Type, b:Type, s:Subst): Subst = {
    //println("calculating mgu for: " + (a, b, s))
    val result = (subs(a, s), subs(b, s)) match {
      case (TyVar(ta), TyVar(tb)) if ta == tb => s
      // this does the 'occurs' check for infinite types.
      case (TyVar(ta), _) if (! getTVarsOfType(b).contains(ta)) =>
        extend(TyVar(ta), b, s)
      case (_, TyVar(_)) => mgu(b, a, s)
      case (TyLam(a1, b1), TyLam(a2, b2)) => mgu(a1, a2, mgu(b1, b2, s))
      case (TyCon(name1, args1), TyCon(name2, args2)) if name1 == name2 =>
        args1.zip(args2).foldLeft(s){ case (sp, (t1, t2)) => mgu(t1, t2, sp) }
      case (x, y) => sys.error("unable to unify: " + (x, y))
    }
    //println("mgu = " + result)
    result
  }

  // Calculate the principal type scheme for an expression in a given
  // typing environment
  def tp(env: Env, exp: Exp, bt: Type, s: Subst): Subst = exp match {
    case Lit(v)  => mgu(litToTy(v), bt, s)
    case i@Id(n) => env.get(i).map{
      case (t,_) => mgu(subs(t, s), bt, s)
    }.getOrElse(sys.error("unknown id: " + n))
    case Lam(x, e) =>
      val a = newTypVar()
      val b = newTypVar()
      tp(env + (x -> (a, Set())), e, b, mgu(bt, TyLam(a, b), s))
    case App(e1, e2) =>
      val a = newTypVar()
      tp(env, e2, a, tp(env, e1, TyLam(a, bt), s))
  }

  var ts = Iterator.from(0)
  def resetTypCounter() = ts = Iterator.from(0)
  def newTypVar() = TyVar("t" + ts.next())

  // the top level type check function
  def typeCheck(exp: Exp): Type = {
    resetTypCounter()
    val a = TyVar("init")
    val s1: Subst = tp(predef, exp, a, Map())

    // rename all the type variables starting from t0
    // this cleans things up a bit as far as presentation goes.
    def renameTyVars(t:Type): Type = {
      val count = Iterator.from(0)
      val m = collection.mutable.Map[String, String]()
      def renameTyVarsHelper(t:Type): Type = t match {
        case TyVar(oldName) => TyVar(m.getOrElseUpdate(oldName, {
          val newName = "t" + count.next()
          m += (oldName -> newName)
          newName
        }))
        case TyLam(a, b) => TyLam(renameTyVarsHelper(a), renameTyVarsHelper(b))
        case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(renameTyVarsHelper(_)))
      }
      renameTyVarsHelper(t)
    }
    renameTyVars(subs(a, s1))
  }
}