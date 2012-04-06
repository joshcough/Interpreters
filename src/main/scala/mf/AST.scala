package mf

object AST {

  /** Term Tree **/
  trait Literal
  case class Num(i: Int) extends Literal
  case class Bool(b: Boolean) extends Literal

  sealed trait Exp
  case class Name(name: String) extends Exp
  case class Lam(arg: Name, body: Exp) extends Exp
  case class App(f: Exp, arg: Exp) extends Exp
  case class Lit(l: Literal) extends Exp
  case class ConstructorApp(name: Name, arguments: List[Exp]) extends Exp
  //TODO: case class Let(name: Name, e: Exp, body: Exp)


  /** Top Level Program Declarations **/
  sealed trait Declaration
  case class Constructor(name: Name, arguments: List[Type])
  case class DataDef(name: Name, typeVars: List[TyVar], constructors: List[Constructor]) extends Declaration
  // I used to have Def, but Def is just Val in disguise...
  // its body is always a lambda.
  case class Val(name: Name, body: Exp) extends Declaration
  case class Program(body: List[Declaration])

  /** Type Tree **/
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
  case class TypeCheckedProgram(types: Map[Name,Type])

  // TODO: review needing to put parens around type arguments that
  // have further type arguments, to avoid ambiguity. For example: 
  // Maybe (Maybe 'a) vs. Maybe Maybe a
  // The first is correct, the second is just weird.
  case class TyCon(name: Name, args: List[Type]) extends Type {
    override def toString = (name.name + " " + args.mkString(" ")).trim
  }
  case class TyForall(tvs: List[TyVar], t: Type) extends Type {
    override def toString = "forall " + tvs.mkString(",") + " . " + t
    // TODO: this is traversing the tree N times unnecessarily.
    def swapAll(newTyVars: List[TyVar]): Type = tvs.zip(newTyVars).foldLeft[Type](t){
      case (newT, (oldTv, newTv)) => swapTyVars(newT, oldTv, newTv)
    }
  }

  // Subtitutions
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

  // newTv had better be fresh here...
  // it had better not be captured by tvs in the forall.
  def swapTyVars(t: Type, oldTv: TyVar, newTv: TyVar): Type = t match {
    case t@TyVar(_)  => if(t == oldTv) newTv else t
    case TyLam(a, r) => TyLam(swapTyVars(a, oldTv, newTv), swapTyVars(r, oldTv, newTv))
    case TyCon(name, tyArgs) => TyCon(name, tyArgs.map(swapTyVars(_, oldTv, newTv)))
    case TyForall(tvs, t) => TyForall(tvs, swapTyVars(t, oldTv, newTv))
  }
  
  // Environments
  type Env = Map[Name, Type]

  def getTVarsOfType(t: Type): Set[String] = t match {
    case TyVar(n)       => Set(n)
    case TyLam(t1, t2)  => getTVarsOfType(t1) ++ getTVarsOfType(t2)
    case TyCon(_, args) => args.flatMap(t => getTVarsOfType(t)).toSet
  }

  val IntT  = TyCon(Name("Int"),  Nil)
  def litToTy(l: Literal): Type = l match { case Num(_)  => IntT }
}
