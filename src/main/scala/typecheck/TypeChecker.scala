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

  def success(s: Subst): Either[String, Subst] = Right(s)

  def typeError(msg: String): Either[String, Subst] = Left(msg)

  // Calculate the principal type scheme for an expression in a given
  // typing environment

  case class TypeCheckState(tyVarCount: Int, env: Env)

  type S[T] = State[TypeCheckState, T]
  type ETS[T] = EitherT[S, String, T]
  type ETSType = ETS[Type]
  type ETSSubst = ETS[Subst]

  def tp(exp: Exp, bt: Type, s: Subst): ETSSubst = {
    def liftES(e: Either[String, Subst]): ETSSubst = eitherT[S, String, Subst](state(e))
    def newTypVar: ETSType = {
      val next = for (n <- modify[TypeCheckState](t =>
        t.copy(tyVarCount = t.tyVarCount + 1))) yield TyVar("t" + n)
      eitherT[S, String, Type]((next).map(t => (Right(t): Either[String, Type])))
    }
    def getEnv = eitherT[S, String, Env](for{
      t <- init[TypeCheckState]
    } yield Right(t.env))
    def updateEnv(id:Id, scheme:TyScheme) = eitherT[S, String, TypeCheckState](
      for(e <- modify[TypeCheckState](t => t.copy(env = t.env + (id -> scheme))))
        yield Right(e)
    )
    def find(id:Id, e:Env) = e.get(id).map {
      case (t, _) => mgu(subs(t, s), bt, s)
    }.getOrElse(Left("unknown id: " + id.name))
    
    exp match {
      case Lit(v) => liftES(mgu(litToTy(v), bt, s))
      case id@Id(n) => for {
        e <- getEnv
        s <- liftES(find(id, e))
      } yield s
      case Lam(x, e) => for {
        a   <- newTypVar
        b   <- newTypVar
        s1  <- liftES(mgu(bt, TyLam(a, b), s))
        _   <- updateEnv(x, (a, Set()))
        res <- tp(e, b, s1)
      } yield res
      case App(e1, e2) => for {
        a   <- newTypVar
        f   <- tp(e1, TyLam(a, bt), s)
        res <- tp(e2, a, f)
      } yield res
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
    tp(exp, a, Map()).run(TypeCheckState(0, predef))._1.right.map(s => renameTyVars(subs(a, s)))
  }
  
//  def typeCheck(s:String): Either[String, Type] = {
//    for(p <- Parser.parse(s)) yield p
//  }
}
