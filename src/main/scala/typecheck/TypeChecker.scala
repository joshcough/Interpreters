package typecheck

import parser.Parser

/**
 * Notes:
 *
 * A lot of this code is a port from:
 *   http://fsharpcode.blogspot.com/2010/08/hindley-milner-type-inference-sample.html
 *
 * The monad transformation stuff is inspired by:
 *   http://www.grabmueller.de/martin/www/pub/Transformers.pdf
 *
 * Robby Findler's lecture notes on type inference can be found here:
 *   http://www.eecs.northwestern.edu/~robby/courses/321-2012-winter/lecture16.pdf
 */
object TypeChecker {

  import TypeCheckerWithInferenceAST._

  import scalaz.State._
  import scalaz.std.list.{listInstance => listTraverse}
  import scalaz.{State, EitherT}
  import EitherT._
  import scalaz.std.either._
  import scalaz.syntax.monad._

  // Unification

  // Calculate the most general unifier of two types,
  // raising a UnificationError if there isn't one
  def mgu(a: Type, b: Type, s: Subst): Either[String, Subst] = {
    //println("calculating mgu for: " + (a, b, s))
    val result = (subs(a, s), subs(b, s)) match {
      case (TyVar(ta), TyVar(tb)) if ta == tb => success(s)
      // this does the 'occurs' check for infinite types.
      case (TyVar(ta), _) if (!getTVarsOfType(b).contains(ta)) =>
        success(extend(ta, b, s))
      case (_, TyVar(_)) => mgu(b, a, s)
      case (TyLam(a1, b1), TyLam(a2, b2)) => for {
        s1 <- mgu(b1, b2, s)
        s2 <- mgu(a1, a2, s1)
      } yield s2
      case (TyCon(name1, args1), TyCon(name2, args2)) if name1 == name2 =>
        // TODO: find out if there is a nicer way to do this...
        args1.zip(args2).foldLeft(success(s)) {
          case (sp, (t1, t2)) =>
            for {s1 <- sp; s2 <- mgu(t1, t2, s1)} yield s2
        }
      case (x, y) => typeError("unable to unify: " +(x, y))
    }
    //println("mgu = " + result)
    result
  }

  def success[T](t: T): Either[String, T] = Right(t)
  def typeError(msg: String): Either[String, Subst] = Left(msg)

  /**
   * State related code
   */
  case class TypeCheckState(tyVarCount: Int, env: Env, subst: Subst)

  type S[T] = State[TypeCheckState, T]
  type ETS[T] = EitherT[S, String, T]

  // get things out of the state
  def mapState[T](f: TypeCheckState => T) =
    eitherT[S, String, T](for(t <- init.map(f)) yield Right(t) : Either[String, T])
  def getEnv   = mapState(_.env)
  def getSubst = mapState(_.subst)

  def find(id: Id, e: Env, bt: Type, s: Subst) = e.get(id).map {
    case (t, _) => mgu(subs(t, s), bt, s)
  }.getOrElse(typeError("unknown id: " + id.name))

  // update things in the state
  def modState(f: TypeCheckState => TypeCheckState) = modify[TypeCheckState](f)
  def newTypVar: ETS[Type] = {
    val next = for (n <- modState(t => t.copy(t.tyVarCount + 1))) yield TyVar("t" + n)
    eitherT[S, String, Type](next.map(t => (Right(t): Either[String, Type])))
  }
  def updateEnv(id:Id, scheme:TyScheme) = mapState(t => t.copy(env = t.env + (id -> scheme)))
  def updateSubst(f: Subst => ETS[Subst]) = {
    def setSubst(newSubst: Subst) = mapState(t => t.copy(subst = newSubst))
    for { s  <- getSubst; s1 <- f(s); _  <- setSubst(s1) } yield s1
  }

  /**
   * Calculate the principal type scheme for an expression in a given
   * typing environment
   */
  def tp(exp: Exp, bt: Type): ETS[Subst] = {
    implicit def unitE(e: Either[String, Subst]): ETS[Subst] = eitherT[S, String, Subst](state(e))
    exp match {
      case Lit(v) => updateSubst(mgu(litToTy(v), bt, _))
      case id@Id(n) => for {
        e <- getEnv
        s <- updateSubst(find(id, e, bt, _))
      } yield s
      case Lam(x, e) => for {
        a <- newTypVar
        b <- newTypVar
        _ <- updateSubst(mgu(bt, TyLam(a, b), _))
        _ <- updateEnv(x, (a, Set()))
        s <- updateSubst(_ => tp (e, b))
      } yield s
      case App(e1, e2) => for {
        a <- newTypVar
        _ <- updateSubst(_ => tp(e1, TyLam(a, bt)))
        s <- updateSubst(_ => tp(e2, a))
      } yield s
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
  def typeCheck(exp: Exp): Either[String, Type] = {
    val a = TyVar("init")
    tp(exp, a).run(TypeCheckState(0, predef, Map()))._1.right.map(s => renameTyVars(subs(a, s)))
  }
  
//  def typeCheck(s:String): Either[String, Type] = {
//    def liftEP(e: Either[String, Program]): ETS[Program] = eitherT[S, String, Program](state(e))
//    val x = for {
//      p <- liftEP(Parser.parse(s))
//      val tvs = p.exps.zip(Stream.from(0).map((i:Int) => TyVar("t" + i))).toList
//      res <- listTraverse.traverseS(tvs){ t =>
//        tp(t._1, t._2)
//      }
//    } yield res
//
//    //def traverseS[S, A, B](fa : F[A])(f : scala.Function1[A, scalaz.State[S, B]]) : scalaz.State[S, F[B]]
//    eitherMonad.traverseS()
//
//    sys.error("todo")
//  }
}


//def unit[T](t: T): ETS[T] = eitherT[S, String, T](state(Right(t)))
