package typecheck

/**
 * Note: A lot of this code is a port from:
 *   http://fsharpcode.blogspot.com/2010/08/hindley-milner-type-inference-sample.html
 */
object TypeCheckerWithInference_Monadic {

  import TypeCheckerWithInferenceAST._
  import scalaz.State
  import scalaz.State._
  import scalaz.std.list.{listInstance => listTraverse}

  // Unification

  // Calculate the most general unifier of two types,
  // raising a UnificationError if there isn't one
  def mgu(a: Type, b: Type, s: Subst): Subst = {
    //println("calculating mgu for: " + (a, b, s))
    val result = (subs(a, s), subs(b, s)) match {
      case (TyVar(ta), TyVar(tb)) if ta == tb => s
      // this does the 'occurs' check for infinite types.
      case (TyVar(ta), _) if (!getTVarsOfType(b).contains(ta)) =>
        extend(TyVar(ta), b, s)
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
      case Lit(v)  => state(mgu(litToTy(v), bt, s))
      case i@Id(n) => state(env.get(i).map {
        case (t, _) => mgu(subs(t, s), bt, s)
      }.getOrElse(sys.error("unknown id: " + n)))
      case Lam(x, e) => for {
        a <- newTypVar
        b <- newTypVar
        t <- tp(env + (x ->(a, Set())), e, b, mgu(bt, TyLam(a, b), s))
      } yield t
      case App(e1, e2) => for {
        a <- newTypVar
        f <- tp(env, e1, TyLam(a, bt), s)
        t <- tp(env, e2, a, f)
      } yield t
    }
  }

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
