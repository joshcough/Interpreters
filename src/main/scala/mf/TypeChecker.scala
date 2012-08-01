package mf

import AST._

import scalaz.State._
import scalaz.std.list.{listInstance => listTraverse}
import scalaz.{State, EitherT}
import EitherT._
import scalaz.std.either._
import scalaz.syntax.monad._

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
 *
 * TODO: TypeChecker uses parser, but it probably shouldn't. It should just get the AST.
 */
object TypeChecker {

  /* Subtitutions */
  type Subst = Map[TyVar, Type]
  def extend(tv: TyVar, t: Type, subs: Subst): Subst = subs + (tv -> t)
  def lookup(tv: TyVar, subs: Subst) = subs.getOrElse(tv, tv)
  // this is 'chase', i believe.
  def subs(t: Type, s: Subst): Type = t match {
    case t@TyVar(_)  => if(t == lookup(t, s)) t else subs(lookup(t, s), s)
    case TyLam(a, r) => TyLam(subs(a, s), subs(r, s))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(subs(_, s)))
    case TyForall(tvs, t) => TyForall(tvs, subs(t, s))
  }

  /* Environments */
  type TypeEnv = Map[Name, Type]

  def getTVarsOfType(t: Type): Set[TyVar] = t match {
    case t@TyVar(n)         => Set(t)
    case TyLam(t1, t2)    => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args)   => args.flatMap(t => getTVarsOfType(t)).toSet
    // TODO TODOTODOTODOTODOTODOTODOTODOTODO i think this might be royally busted!
    case TyForall(tvs, t) => getTVarsOfType(t)
  }
  //    for { tvs <- listTraverse.sequenceS(t.tvs.map(_ => newTV)) } yield t.swapAll(tvs)

  /**
   * State related code
   */
  case class TypeCheckState(tyVarCount: Int, env: TypeEnv, subst: Subst){
    def bump = this.copy(tyVarCount = tyVarCount + 1)
  }
  type S[+T] = State[TypeCheckState, T]
  type ETS[+T] = EitherT[S, String, T]

  /**
   * get things out of the state
   */

  def mapState[T](f: TypeCheckState => T) =
    eitherT[S, String, T](for(t <- init.map(f)) yield Right(t) : Either[String, T])
  def getEnv   = mapState(_.env)
  def getSubst = mapState(_.subst)

  def find(id: Name, e: TypeEnv, bt: Type, s: Subst) = e.get(id).map {
    t => mgu(subs(t, s), bt, s)
  }.getOrElse(typeError("unknown id: " + id.name))

  /**
   * update things in the state
   */

  def modAndGet[T](f: T => T): State[T, T] = for { _ <- modify(f); t <- get } yield t
  def modAndGetType = modAndGet[TypeCheckState] _
    
  def liftS[T](s:State[TypeCheckState,T]) =
    eitherT[S, String, T](s.map{ t => Right(t): Either[String, T] })
  /* get a fresh type variable, incrementing the count in the state in the process. */
  def newTypVar: ETS[Type] = liftS(for (n <- modAndGetType(_.bump)) yield TyVar("t" + n.tyVarCount))

  /* update the environment in the state */
  def updateEnv(id:Name, s:Type): ETS[TypeEnv] =
    liftS(modAndGetType(t => t.copy(env = t.env + (id -> s))).map(_.env))

  /**
   * run a function that produces a new subst
   * and update the state with that new subst afterwards
   */
  def updateSubst(f: Subst => ETS[Subst]): ETS[Subst] = {
    /** change the subst in the state **/
    def setSubst(newSubst: Subst): ETS[Subst] =
      liftS(modAndGetType(t => t.copy(subst = newSubst)).map(_.subst))
    for { s  <- getSubst; s1 <- f(s); _  <- setSubst(s1) } yield s1
  }

  def success(s: Subst): ETS[Subst] = eitherT[S, String, Subst](state(Right(s)))
  def typeError(msg: String): ETS[Subst] = eitherT[S, String, Subst](state(Left(msg)))

  /** Unification **/

  /**
   * Calculate the most general unifier of two types,
   * raising a UnificationError if there isn't one
   */
  def mgu(a: Type, b: Type, s: Subst): ETS[Subst] = {
    //println("calculating mgu for: " + (a, b, s))
    val sub1 = subs(a, s)
    val sub2 = subs(b, s)
    //println("(sub1, sub2) = " + (sub1, sub2))
    (sub1, sub2) match {
      case (tf@TyForall(_, _), t2) => for{
        t  <- liftS(freshen(tf))
        s1 <- mgu(t, t2, s)
      } yield s1
      case (t1, tf@TyForall(_, _)) => for{
        t  <- liftS(freshen(tf))
        s1 <- mgu(t1, t, s)
      } yield s1
      case (TyVar(ta), TyVar(tb)) if ta == tb => success(s)
      /** this does the 'occurs' check for infinite types. **/
      case (t@TyVar(_), _) if (!getTVarsOfType(b).contains(t)) => success(extend(t, b, s))
      case (_, TyVar(_)) => mgu(b, a, s)
      case (TyLam(a1, b1), TyLam(a2, b2)) => for {
        s1 <- mgu(b1, b2, s)
        s2 <- mgu(a1, a2, s1)
      } yield s2
      case (TyCon(name1, args1), TyCon(name2, args2)) if name1 == name2 =>
        // TODO: find out if there is a nicer way to do this...
        // TODO: definitely not checking if we have the same number of args here...
        args1.zip(args2).foldLeft(success(s)) {
          case (sp, (t1, t2)) =>
            for {s1 <- sp; s2 <- mgu(t1, t2, s1)} yield s2
        }
      case (x, y) => typeError("unable to unify: " +(x, y))
    }
  }

  def freshen(t:TyForall): S[Type] = {
    // TODO: UGH! This is duplicated. ok for now, just trying to get this to compile.
    def newTV = for (n <- modAndGetType(t => t.copy(t.tyVarCount + 1))) yield TyVar("t" + n.tyVarCount)
    for { tvs <- listTraverse.sequenceS(t.tvs.map(_ => newTV)) } yield t.swapAll(tvs)
  }

  /**
   * Calculate the principal type scheme for an expression
   */
  def tp(exp: Exp, bt: Type): ETS[Subst] = {
//    println("tp: " + (exp, bt))
    implicit def unitE(e: Either[String, Subst]): ETS[Subst] = eitherT[S, String, Subst](state(e))
    exp match {
      case Lit(v) => updateSubst(mgu(litToTy(v), bt, _))
      case id@Name(n) => for {
        e <- getEnv
        s <- updateSubst(find(id, e, bt, _))
      } yield s
      case Lam(x, e) => for {
        a <- newTypVar
        b <- newTypVar
        _ <- updateSubst(mgu(bt, TyLam(a, b), _))
        _ <- updateEnv(x, a)
        s <- updateSubst(_ => tp (e, b))
      } yield s
      case App(e1, e2) => for {
        a <- newTypVar
        _ <- updateSubst(_ => tp(e1, TyLam(a, bt)))
        s <- updateSubst(_ => tp(e2, a))
      } yield s
    }
  }

  /**
   * rename all the type variables starting from t0
   * this cleans things up a bit as far as presentation goes.
   */
  def renameTyVars(t: Type): Type = {
    type R = (Int, Map[String, String])
    def update(k: String, r: R) =
      if (r._2.contains(k)) r else (r._1 + 1, r._2 + (k -> ("t" + r._1)))
    def helper(t: Type): State[R, Type] = t match {
      case TyVar(oldName) =>
        for {z <- modAndGet[R](update(oldName, _))} yield TyVar(z._2(oldName))
      case TyLam(a, b) =>
        for {t1 <- helper(a); t2 <- helper(b)} yield TyLam(t1, t2)
      case TyCon(name, tyArgs) =>
        for {ts <- listTraverse.traverseS(tyArgs)(helper(_))} yield TyCon(name, ts)
      // TODO: come back and finish this later...
      case tf@TyForall(tvs, t) => state(tf)
//        val newTvs = listTraverse.sequenceS(tvs.map)
//        for {ts <- listTraverse.traverseS(tyArgs)(helper(_))} yield TyCon(name, ts)
    }
    helper(t)((0, Map[String, String]()))._2
  }

  // the top level type check function *for a single expression only*
  def typeCheckExp(exp: String, predef: TypeEnv): Either[String, Type] = {
    def inner(exp: Exp, tcs: TypeCheckState): Either[String, Type] = {
      val a = TyVar("init")
      val as = tp(exp, a).run(tcs)
      as._2.right.map(s => renameTyVars(subs(a, s)))
    }
    for {
      e   <- Parser.parseExpr(exp)
      res <- inner(e, TypeCheckState(0, predef, Map()))
    } yield res
  }

  /**
    i need to take each of my expressions and its type variable
    and run that through tp. this will give back an
    EitherT[State[TypeCheckState, Subst], String, Subst]
    i think something is most definitely wrong with that type,
    but i'll come back to that later.
    the idea is to run them all, and get back a bunch of state objects,
    but im not sure how to do that yet.
    once i have the state objects, i can chain them all together.
    i was hoping to use sequence or traverse for this.
    anyway, i have enough info here to call tp on each of expsAndTypeVars.
    so that is a start.
   */
  def typeCheck(s:String): Either[String, TypeEnv] = for {
    p <- Parser.parseProgram(s)
    t <- typeCheck(p)
  } yield t

  /**
   * TODO: not sure how its even possible to have a bare type here
   * TODO: and not an Either. I think I do have an either, but I'm just
   * TODO: pulling the subst from the state, and not checking to see
   * TODO: if the result is a Left or Right.
   * TODO: I should just use the subst from the result, not the state.
   */
  def typeCheck(p: Program, predef: TypeEnv = Map()): Either[String, TypeEnv] = {
    // println(p)

    //TODO: i am not dealing with data defs here yet.

    /**
     * When type checking a DataDef I need to:
     * - Create a type constructor TyCon
     * - foreach constructor * typeAgs, generate the equivalent of:
     *   def conName = Forall tyArgs . TyLam conArg1 -> ... TyLam conArgN -> TyCon
     */
    def makeTyCons(d:DataDef): List[(Name, TyForall)] = {
      val tyCon = TyCon(d.name, d.typeVars)
      def makeTyLam(c:Constructor) = (c.name, TyForall(d.typeVars, c.arguments.foldRight[Type](tyCon){
        case (nextArg, t) => TyLam(nextArg, t)
      }))
      d.constructors.map(makeTyLam)
    }

    val tylams: List[(Name, TyForall)] = p.body.collect{ case d: DataDef => d }.flatMap(makeTyCons)

    /* make up frest type variables for each def, and the final expression */
    def tvSupply = Stream.from(0).map((i:Int) => TyVar("t" + i))


    val valsAndTyVars = p.body.collect{ case v: Val => v }.zip(tvSupply).toList
    //println(valsAndTypeVars)

    /* create the initial type environment */
    // val env = valsAndTyVars.map{ case (v,tv) => (v.name, tv) }.toMap ++ tylams.toMap ++ predef
    val env = tylams.toMap ++ predef
    //println("initial env: " + env)

    // TODO: make sure there are no duplicate names defined...
    // TODO: also make sure that nothing defined in the body is also in predef.
    // TODO: maybe i should just get rid of predef altogether.

    /* run everything with a base state. */
    val s = listTraverse.sequenceS(valsAndTyVars.map{ case (v, tv) =>
      for {
        s <- tp(v.body, tv)
        // Top Level Defs must be generalized to Forall here!
        val frees = getTVarsOfType(s(tv))
        _ <- updateEnv(v.name, TyForall(frees.toList, s(tv)))
      } yield s
    }.map(_.run))

    //println(s)
    val r = s(TypeCheckState(valsAndTyVars.length, env, Map()))
    val finalSubstE = r._2.last

    /* finally, rename all the type variables so that they look nice */

    // TODO: I'm not including the data defs in this!
    def createResult(subst:Subst) =
      // TODO: this renaming might need to be fixed, because it starts over
      // TODO: with each expression...but this might be ok. look into it.
      valsAndTyVars.map{ case (v, tv) => (v.name, renameTyVars(subs(tv, subst))) }.toMap

    for{ subst <- finalSubstE } yield createResult(subst)
  }
}
