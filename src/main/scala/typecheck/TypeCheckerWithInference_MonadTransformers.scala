package typecheck

/**
 * Notes:
 *
 * A lot of this code is a port from:
 * http://fsharpcode.blogspot.com/2010/08/hindley-milner-type-inference-sample.html
 *
 * The monad transformation stuff is inspired by:
 * http://www.grabmueller.de/martin/www/pub/Transformers.pdf
 *
 * Robby Findler's lecture notes on type inference can be found here:
 * http://www.eecs.northwestern.edu/~robby/courses/321-2012-winter/lecture16.pdf
 */
object TypeCheckerWithInference_MonadTransformers {

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
        success(extend(TyVar(ta), b, s))
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

  type S[+T] = State[Int, T]
  type ETS[+T] = EitherT[S, String, T]
  type ETSType = ETS[Type]
  type ETSSubst = ETS[Subst]

  def tp(env: Env, exp: Exp, bt: Type, s: Subst): ETSSubst = {
    def liftES(e: Either[String, Subst]): ETSSubst = eitherT[S, String, Subst](state(e))
    def newTypVar: ETSType = eitherT[S, String, Type]((for (n <- modify[Int](_ + 1)) yield
      TyVar("t" + n)).map(t => (Right(t): Either[String, Type])))
    exp match {
      case Lit(v) => liftES(mgu(litToTy(v), bt, s))
      case i@Name(n) => liftES(env.get(i).map {
        t => mgu(subs(t, s), bt, s)
      }.getOrElse(Left("unknown id: " + n)))
      case Lam(x, e) => for {
        a <- newTypVar
        b <- newTypVar
        s1 <- liftES(mgu(bt, TyLam(a, b), s))
        res <- tp(env + (x -> a), e, b, s1)
      } yield res
      case App(e1, e2) => for {
        a <- newTypVar
        f <- tp(env, e1, TyLam(a, bt), s)
        res <- tp(env, e2, a, f)
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
        for {_ <- modify[R](update(oldName, _)); z <- get} yield TyVar(z._2(oldName))
      case TyLam(a, b) =>
        for {t1 <- helper(a); t2 <- helper(b)} yield TyLam(t1, t2)
      case TyCon(name, tyArgs) =>
        for {ts <- listTraverse.traverseS(tyArgs)(helper(_))} yield TyCon(name, ts)
    }
    helper(t)((0, Map[String, String]()))._2
  }

  // the top level type check function
  def typeCheck(exp: Exp): Either[String, Type] = {
    val a = TyVar("init")
    for {s <- tp(predef, exp, a, Map()).run(0)._2} yield renameTyVars(subs(a, s))
  }
}


// Lots of failed attempts that might be useful in explaining thought process:


//    def r(a:Type, b: Type, x: String, e: Exp, ss: Either[String, Subst]):
//      Either[String, State[Int, Either[String, Subst]]] =
//      for { s1: Subst <- ss } yield tp(env + (x -> (a, Set())), e, b, s1)

//    def wtf(e: Either[String, V]): V = e match {
//      case Left(m) => Left(m)
//      case Right(v) => v
//    }


//eitherMonad.bind(Right(newTypVar)){(t: State[Int, Type]) =>
//val e: Either[String, State[Int, Subst]] = fromMgu(litToTy(v), bt)
//eitherMonad.map(e){ st => st
//// this can get us closer...but the outer stateMonad bind needs a state here
//// the whole expression needs an Either. thats where the disconnect is.
//}
//}
//
//
////        stateMonad.bind(newTypVar){(t: Type) =>
////          val e: Either[String, State[Int, Subst]] = fromMgu(litToTy(v), bt)
////          // obviously this doesnt work...using stateMonad on an Either...
////          val q = stateMonad.bind(e)(x => x)
////          // what happens if we try to use eitherMonad
////          val xxx = eitherMonad.bind(e){ st =>
////            // this can get us closer...but the outer stateMonad bind needs a state here
////            // the whole expression needs an Either. thats where the disconnect is.
////          }
////        }
//
////        for{
////          _: Type <- newTypVar
////          x <- mgu(litToTy(v), bt, s)
////        } yield x


//  type V = Either[String, State[Int, Subst]]
//  type X[A] = State[Int, A]
//  type Y = EitherT[X, String, State[Int, Subst]]

//  def stfn(e: Either[String, Value]) = (s: Int) => id[(Either[String, Value], Int)](e, s+1)
//
//  def eitherNStateT[T](e: Either[String, T]) =
//    eitherT[ETS, String, T](state[Int, Either[String, Value]](stfn(e)))


//    object EitherT extends java.lang.Object with scalaz.EitherTFunctions with
//      scalaz.EitherTInstances with scala.ScalaObject {
//      def apply[F[_], A, B](a : F[scala.Either[A, B]]) : scalaz.EitherT[F, A, B] = { /* compiled code */ }
//def bind[A, B](fa : scalaz.EitherT[F, E, A])
//              (f : scala.Function1[A, scalaz.EitherT[F, E, B]]) : scalaz.EitherT[F, E, B]

//fromMgu(litToTy(v), bt)
//        val et: EitherT[X,String,State[Int, Subst]] = EitherT(Right(newTypVar))
//        eitherTMonad(stateMonad).bind(et) { (st: State[Int, Type]) => Right(st)
//        }

//      eitherMonad.bind(Right(newTypVar)){(t: State[Int, Type]) =>
//        val e: Either[String, State[Int, Subst]] = fromMgu(litToTy(v), bt)
//          eitherMonad.map(e){ st => st
//          // this can get us closer...but the outer stateMonad bind needs a state here
//          // the whole expression needs an Either. thats where the disconnect is.
//        }
//      }
//

//      for{
//        s1: State[Int, Type] <- liftET(newTypVar)
//        s2: State[Int, Type] <- liftET(for{ t <- s1; t2 <- newTypVar } yield t2)
//        x <- fromMgu(litToTy(v), bt)
//      } yield x

//    def newTypVar: S[Type] = for(n <- modify[Int](_ + 1)) yield TyVar("t" + n)
//    def fromMgu(t1:Type, t2:Type): V = for{ x <- mgu(t1, t2, s) } yield state(x)
//    //def liftET[T](t:State[Int, T]): Either[String, State[Int, T]] = Right(t)
//    //def apply[F[_], A, B](a : F[scala.Either[A, B]]) : scalaz.EitherT[F, A, B]
//    def liftET[T](t:T): EitherT[X, String, T] = EitherT(state(Right(t)))


//  for{
//    _: State[Int, Type] <- liftE(newTypVar)
//    x <- fromMgu(litToTy(v), bt)
//  } yield x


//      case Lam(x, e) => for {
//        st1: State[Int, Type]  <- liftET(newTypVar)
//        st2: State[Int, (Type, Type)]  <- liftET(for{ t1 <- st1; t2 <- newTypVar } yield (t1,t2))
//        ss:  State[Int, Subst] <- liftET(for{ ab <- st2; x <- fromMgu(bt, TyLam(ab._1, ab._2)) } yield x)
//        t <- fromMgu(BoolT, bt)
////        t <- for { s1: Subst <- ss } yield tp(env + (x -> (a, Set())), e, b, s1)
//      } yield t


//        eitherTMonad(stateMonad).bind(newTypVar){ t =>
//          liftES(mgu(BoolT, bt, s))
//        }

//        for {
//          t <- newTypVar
//        } sys.error("hi")
