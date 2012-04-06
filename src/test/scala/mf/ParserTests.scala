package mf

import org.scalacheck.Properties
import AST._


trait ParseTestHelpers extends util.Compare { self: Properties =>
  implicit def stringToName(s:String) = Name(s)
  implicit def intToLit(i:Int) = Lit(Num(i))

  val id = Lam("x", "x")
  val idDef = Val("id", id)
  val (a,b,c) = (TyVar("a"), TyVar("b"), TyVar("c"))

  def parseType(typeString: String, expectedTypeString:Option[String]=None) = compareR(
    typeString,
    Parser.parseType(typeString).right.map(_.toString),
    expectedTypeString.getOrElse(typeString)
  )
  def parseExp(code: String, expected: Exp) = compareR(code, Parser.parseExpr(code), expected)
  def parseData(code: String, expected: DataDef) = compareR(code, Parser.parseData(code), expected)
  def parseProgram(code: String, expected: Program) = compareR(code, Parser.parseProgram(code), expected)
}

object TypeParserTests extends Properties("Type Parser") with ParseTestHelpers {
  parseType("Int")
  parseType("Int -> Int")
  parseType("Int -> Bool -> Int")
  parseType("'t0 -> 't0 -> 't0")
  parseType("'t0 -> 't1")
  parseType("'t0")
  parseType("'t0 -> Bool")
  parseType("('t0 -> 't1) -> ('t1 -> 't2) -> 't0 -> 't2")
  parseType("(('t1 -> 't2) -> 't1 -> 't2) -> 't2")
  parseType("('t1 -> ('t2 -> 't1) -> 't2) -> 't2")
}

object ExpressionParserTests extends Properties("Expression Parser") with ParseTestHelpers {
  parseExp("6",          Lit(Num(6)))
  parseExp("( x -> x )", id)
  parseExp("(x->x)",     id)
  parseExp("(x x)",      App("x","x"))
  parseExp("(x x')",     App("x","x'"))
  parseExp("(x x'')",    App("x","x''"))
  parseExp("(x x'p')",   App("x","x'p'"))
}

object DataParserTests extends Properties("Program Parser") with ParseTestHelpers {

  parseData("(data Bool () ((True) (False)))",
    DataDef("Bool", List(),
      List(
        Constructor("True", Nil),
        Constructor("False", Nil))
    )
  )

//  val listCon = TyCon("List", List(a))
//  parseData("(data List ('a) ((Nil) (Cons 'a (List 'a))))",
//    DataDef("List", List(a),
//      List(
//        Constructor("Nil", Nil),
//        Constructor("Cons", List(a, listCon))
//      )
//    )
//  )

  /**
   * When type checking a DataDef I need to:
   *   - Create a type constructor, like xCon below.
   *   -
   */
  val xCon = TyCon("X", List(a, b, c))
  // data X a b c = A a | B b | C c
  parseData("(data X ('a 'b 'c) ((A 'a) (B 'b) (C 'c)))",
    DataDef("X", List(a, b, c),
      List(
        Constructor("A", List(a)),
        Constructor("B", List(b)),
        Constructor("C", List(c))
      )
    )
  )
}

object ProgramParserTests extends Properties("Program Parser") with ParseTestHelpers {
  def res(e:Exp) = Val("res", e)
  parseProgram("(def id (x -> x)) (val res 7)", Program(List(idDef, res(7))))
  parseProgram("(def id (x -> x)) (val res (x -> x))", Program(List(idDef, res(id))))
  parseProgram("(def id (x -> x)) (def id (x -> x)) (val res (x -> x))", Program(List(idDef, idDef, res(id))))
}
