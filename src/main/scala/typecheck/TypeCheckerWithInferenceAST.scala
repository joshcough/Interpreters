package typecheck

object TypeCheckerWithInferenceAST {

  // AST
  trait Literal
  case class Num(i:Int) extends Literal
  case class Bool(b:Boolean) extends Literal

  trait Exp
  case class Id(name:String) extends Exp
  case class Lam(arg: Id, body: Exp) extends Exp
  case class App(f:Exp, arg: Exp) extends Exp
  case class Lit(l:Literal) extends Exp
  //  Let      of string * Exp * Exp   // local definition

  // Type Tree
  trait Type
  case class TyLam(f:Type, arg:Type) extends Type {
    override def toString = f.toString + " -> " + arg.toString
  }
  case class TyVar(name:String) extends Type {
    override def toString = "'" + name
  }
  case class TyCon(name:String, args: List[Type]) extends Type {
    override def toString = name
  }

  case class Program(exps: List[Exp])

  // Subtitutions
  type Subst = Map[TyVar, Type]
  def extend(tv:TyVar, t:Type, subs: Subst): Subst = subs + (tv -> t)
  def lookup(tv:TyVar, subs:Subst) = subs.getOrElse(tv, tv)
  def subs(t:Type, s:Subst): Type = t match {
    case t@TyVar(_)  => if(t == lookup(t, s)) t else subs(lookup(t, s), s)
    case TyLam(a, r) => TyLam(subs(a, s), subs(r, s))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(subs(_, s)))
  }

  // Environments
  type TyScheme = (Type, Set[String])
  type Env = Map[Id, TyScheme]
  // TODO: TyScheme is used for Let, which I haven't done yet.
  // This implementation could just have type Env = Map[String, Type]

  def getTVarsOfType(t:Type): Set[String] = t match {
    case TyVar(n)       => Set(n)
    case TyLam(t1, t2)  => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args) => args.flatMap(t => getTVarsOfType(t)).toSet
  }

  val numCon  = TyCon("Num",  Nil)
  val boolCon = TyCon("Bool", Nil)

  def litToTy(l:Literal): Type = l match {
    case Num(_)  => numCon
    case Bool(_) => boolCon
  }

  val predef: Env = Map(
    // TODO: notice that identity is not polymorphic.
    Id("identity") -> (TyLam(numCon, numCon), Set()),
    Id("+")   -> (TyLam(numCon, TyLam(numCon, numCon)), Set()),
    Id("-")   -> (TyLam(numCon, TyLam(numCon, numCon)), Set()),
    Id("==")  -> (TyLam(numCon, TyLam(numCon, boolCon)), Set()),
    Id("and") -> (TyLam(boolCon, TyLam(boolCon, boolCon)), Set()),
    Id("or")  -> (TyLam(boolCon, TyLam(boolCon, boolCon)), Set())
    // TODO: how do we do if? is it like this?
    //  tv => "if" -> (TyLam(boolCon, TyLam(tv, TyLam(tv, tv))), Set())
    // we need to use the type scheme for it somehow. things to learn.
  )
}
