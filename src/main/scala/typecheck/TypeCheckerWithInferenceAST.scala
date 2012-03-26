package typecheck

object TypeCheckerWithInferenceAST {

  // AST
  trait Literal
  case class Num(i:Int) extends Literal
  case class Bool(b:Boolean) extends Literal

  trait Exp
  case class Var(name:String) extends Exp
  case class Lam(arg: String, body: Exp) extends Exp
  case class App(f:Exp, arg: Exp) extends Exp
  case class Lit(l:Literal) extends Exp
  //  Let      of string * Exp * Exp   // local definition

  // Type Tree
  trait Type
  case class TyLam(f:Type, arg:Type) extends Type
  case class TyVar(name:String) extends Type
  case class TyCon(name:String, args: List[Type]) extends Type

  // Subtitutions
  type Subst = Map[String, Type]
  def extend(v:String, t:Type, subs: Subst): Subst = subs + (v -> t)
  def lookup(v:String, subs:Subst) = subs.getOrElse(v, TyVar(v))
  def subs(t:Type, s:Subst): Type = t match {
    case TyVar(n) =>
      val tp = lookup(n, s)
      if(t == tp) tp else subs(tp, s)
    case TyLam(a, r) => TyLam(subs(a, s), subs(r, s))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(subs(_, s)))
  }

  // Environments
  type TyScheme = (Type, Set[String])
  type Env = Map[String, TyScheme]
  // TODO: TyScheme is used for Let, which I haven't done yet.
  // This implementation could just have type Env = Map[String, Type]

  def getTVarsOfType(t:Type): Set[String] = t match {
    case TyVar(n) => Set(n)
    case TyLam(t1, t2) => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args) => args.flatMap(t => getTVarsOfType(t)).toSet
  }

  val numCon = TyCon("Num", Nil)
  val boolCon = TyCon("Bool", Nil)

  def litToTy(l:Literal): Type = l match {
    case Num(_) => numCon
    case Bool(_) => boolCon
  }

  val predef: Env = Map(
    "+" ->   (TyLam(numCon, TyLam(numCon, numCon)), Set()),
    "-" ->   (TyLam(numCon, TyLam(numCon, numCon)), Set()),
    "==" ->  (TyLam(numCon, TyLam(numCon, boolCon)), Set()),
    "and" -> (TyLam(boolCon, TyLam(boolCon, boolCon)), Set()),
    "or" ->  (TyLam(boolCon, TyLam(boolCon, boolCon)), Set())
    // TODO: how do we do if? is it like this?
    //  tv => "if" -> (TyLam(boolCon, TyLam(tv, TyLam(tv, tv))), Set())
  )
}
