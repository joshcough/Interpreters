package typecheck

object TypeCheckerWithInferenceAST {

  // AST
  trait Literal
  case class Num(i: Int) extends Literal
  case class Bool(b: Boolean) extends Literal

  sealed trait Exp
  case class Name(name: String) extends Exp
  case class Lam(arg: Name, body: Exp) extends Exp
  case class App(f: Exp, arg: Exp) extends Exp
  case class Lit(l: Literal) extends Exp

  case class Def(name: Name, lam: Lam)
  object Program {
    def apply(e: Exp): Program = Program(List(), e)
  }

  case class Program(defs: List[Def], e: Exp)


  // Type Tree
  sealed trait Type
  case class TyLam(f: Type, arg: Type) extends Type {
    override def toString = (f match {
      case t@TyLam(_, _) => "(" + t.toString + ")"
      case _ => f.toString
    }) + " -> " + arg.toString
  }
  case class TyVar(name: String) extends Type {
    override def toString = "'" + name
  }
  case class TyCon(name: String, args: List[Type]) extends Type {
    override def toString = (name + " " + args.mkString(" ")).trim
  }

  // Subtitutions
  type Subst = Map[TyVar, Type]
  def extend(tv: TyVar, t: Type, subs: Subst): Subst = subs + (tv -> t)
  def lookup(tv: TyVar, subs: Subst) = subs.getOrElse(tv, tv)

  // this is 'chase', i believe.
  def subs(t: Type, s: Subst): Type = t match {
    case t@TyVar(_)  => if (t == lookup(t, s)) t else subs(lookup(t, s), s)
    case TyLam(a, r) => TyLam(subs(a, s), subs(r, s))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(subs(_, s)))
  }

  // newTv had better be fresh here...
  // it had better not be captured by tvs in the forall.
  def swapTyVars(t: Type, oldTv: TyVar, newTv: TyVar): Type = t match {
    case t@TyVar(_)  => if (t == oldTv) newTv else t
    case TyLam(a, r) => TyLam(swapTyVars(a, oldTv, newTv), swapTyVars(r, oldTv, newTv))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(swapTyVars(_, oldTv, newTv)))
  }

  // Environments
  type Env = Map[Name, Type]

  def getTVarsOfType(t: Type): Set[String] = t match {
    case TyVar(n) => Set(n)
    case TyLam(t1, t2) => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args) => args.flatMap(t => getTVarsOfType(t)).toSet
  }

  val IntT  = TyCon("Int", Nil)
  val BoolT = TyCon("Bool", Nil)

  def litToTy(l: Literal): Type = l match {
    case Num(_)  => IntT
    case Bool(_) => BoolT
  }

  def listCon(a: TyVar) = TyCon("List", List(a))

  val a = TyVar("a")

  val predef: Env = Map(
    Name("identity") -> TyLam(IntT, IntT),
    Name("+") -> TyLam(IntT, TyLam(IntT, IntT)),
    Name("-") -> TyLam(IntT, TyLam(IntT, IntT)),
    Name("==") -> TyLam(IntT, TyLam(IntT, BoolT)),
    Name("and") -> TyLam(BoolT, TyLam(BoolT, BoolT)),
    Name("or") -> TyLam(BoolT, TyLam(BoolT, BoolT))
  )
}
