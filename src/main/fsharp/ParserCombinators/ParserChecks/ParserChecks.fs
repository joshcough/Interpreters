module ParserChecks

open FsCheck
open Parsers
open Conversions

let chooseFromList xs = 
    gen { let! i = Gen.choose (0, List.length xs-1) 
          return (List.nth xs i) }

let getIdChar = chooseFromList (List.append ['a'..'z'] ['A'..'Z'])
let genId = Gen.map (fun cs -> mkString(cs, "")) (Gen.listOf getIdChar) |>
            Gen.suchThat (fun (s:string) -> s.Length > 0 && s.Length < 10) 
let genAtom = Gen.map Atom genId
let genNum() = Gen.map Number Arb.generate<int>
let genStrLit() = 
    Arb.generate<string> |> 
    Gen.suchThat (fun (s:string) -> not(s.Contains("\""))) |>
    Gen.map StrLit 

let genSExpr: Gen<SExpr> =
    let rec sexpr' s = 
        match s with
        | 0 -> Gen.oneof [ genNum(); genAtom; genStrLit() ]
        | n when n>0 -> Gen.oneof [ genNum(); genAtom; Gen.map SList (Gen.listOf (sexpr' (n/5)))]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    FsCheck.Gen.sized sexpr'

type SExprGenerator =
    static member SExpr() = {
        new Arbitrary<SExpr>() with
            override x.Generator = genSExpr
            override x.Shrinker t = Seq.empty 
    }

let parseInvolutive<'a when 'a : equality>(aa:'a, stringVersionOfA, p:Parser<'a>) = p.Parse(stringVersionOfA).Get = aa
let matchCharInvolutive (c:char) = parseInvolutive(c, c.ToString(), matchChar c)
let numberInvolutive (n:int) = parseInvolutive(n, n.ToString(), number)
let numbersInvolutive (ns:List<int>) = parseInvolutive(ns, mkString(ns, " "), numbers)
let sexprInvolutive (s:SExpr) = parseInvolutive(s, s.ToString(), sexpr)

let x() =
    Arb.register<SExprGenerator>()
    FsCheck.Check.Quick matchCharInvolutive
    FsCheck.Check.Quick numberInvolutive
    FsCheck.Check.Quick numbersInvolutive
    FsCheck.Check.Quick sexprInvolutive
    0