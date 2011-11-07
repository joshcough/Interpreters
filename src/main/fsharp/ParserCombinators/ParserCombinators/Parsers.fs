module Parsers
#light

open Conversions

type ParseResult<'a> = 
  | Success of 'a * string
  | Failure of string
  override self.ToString() = match self with
    | Success(v, s) ->  sprintf "Success(%A, %s)" v s
    | Failure(m) -> sprintf "Failure(%s)" m
  member pr.Get = match pr with | Success(a, _) -> a | _ -> failwith("cant get from a failure")
  member pr.Failed = match pr with | Failure _ -> true | _ -> false
  member pr.GetFailureMessage = match pr with | Failure(m) -> m | _ -> failwith("not a failure") 

type Parser<'a> =
    abstract member Parse: string -> ParseResult<'a>

let (^^) (p:Parser<'a>)(f: 'a -> 'b) =  { 
    new Parser<'b> with member this.Parse(s) =  match p.Parse(s) with
        | Success (a, rest) -> Success(f(a), rest)
        | Failure (message) -> Failure(message)
}                 
let (|||) (l:Parser<'a>)(r:Parser<'a>) = {
    new Parser<'a> with member this.Parse(s) = match l.Parse(s) with
        | Success (al, rest) -> Success(al, rest)
        | Failure (leftMessage) -> match r.Parse(s) with
            | Success (ar, rest) -> Success (ar, rest)
            | Failure (rightMessage) -> Failure (leftMessage + " and " + rightMessage)
}
let andThen(l:Parser<'a>,r:Lazy<Parser<'b>>) = {
    new Parser<'a * 'b> with member this.Parse(s) = match l.Parse(s) with
        | Success (al, rest) -> match r.Force().Parse(rest) with
            | Success (ar, rest) -> Success ((al, ar), rest)
            | Failure (message) -> Failure (message)
        | Failure (message) -> Failure (message)
}

let (++) (l:Parser<'a>)(r:Parser<'b>) = andThen(l, lazy(r)) 
let (+++) (l:Parser<'a>)(r:Lazy<Parser<'b>>) = andThen(l,r) 
let (^^^) (p:Parser<'a>)(v:Lazy<'b>) = {
    new Parser<'b> with member this.Parse(s) = match p.Parse(s) with
        | Success (_, rest) -> Success(v.Force(), rest)
        | Failure (message) -> Failure(message)
}
let opt<'a>(p:Parser<'a>) = {
    new Parser<Option<'a>> with member this.Parse(s) = match p.Parse(s) with
        | Success (v, rest) -> Success(Some(v), rest)
        | Failure (message) -> Success(None, s)
}
let always = { new Parser<string> with member p.Parse(s) = Success("", s) }
let never<'a> () = { new Parser<'a> with member p.Parse(s) = Failure("never") }
let rec oneOf<'a> (parsers: List<Parser<'a>>) : Parser<'a> = 
    match parsers with
    | p :: ps -> p ||| oneOf<'a>(ps)
    | _ -> never()
let matchChar (c:char): Parser<char> = {
    new Parser<char> with member p.Parse(s) = 
        if s.Length > 0 && s.[0] = c then Success(c, s.[1..]) else Failure("didn't find: " + c.ToString())
}
let anyCharBut (c:char) : Parser<char> = {
    new Parser<char> with member p.Parse(s) = 
        if s.Length = 0 then Failure("end of stream avoiding: " + c.ToString())
        else if s.[0] = c then Failure("tried to avoid it, but encountered: " + c.ToString())
        else Success(s.[0], s.[1..])
}
let rec matchAll(ps:List<Parser<'a>>): Parser<List<'a>> = match ps with
    | [] -> always ^^^ lazy([])
    | p :: ps' -> p ++ matchAll(ps) ^^ List.Cons
let matchWord (findMe:string): Parser<string> = 
    matchAll(ToList(findMe) |> List.map matchChar) ^^ (fun cs -> mkString(cs, ""))
let rec zeroOrMore<'a> (p:Parser<'a>): Parser<List<'a>> =
    (p +++ lazy(zeroOrMore<'a>(p)) ^^ List.Cons) ||| (always ^^^ lazy([]))
let oneOrMore<'a> (p:Parser<'a>): Parser<List<'a>> = p ++ zeroOrMore<'a>(p) ^^ List.Cons
let repsep<'a, 'b> (pa:Parser<'a>, pb:Parser<'b>) : Parser<List<'a>> =
    zeroOrMore(pa ++ pb ^^ fst) ++ (opt(pa) ^^ Option.toList) ^^ (fun (l, r) -> l @ r)
let surroundedByLazy (l:Parser<'l>, p:Lazy<Parser<'a>>, r:Parser<'r>) = 
    l +++ p +++ lazy(r) ^^ (fun ((_, pr), _) -> pr)
let surroundedBy l p r = surroundedByLazy(l, lazy(p), r)

let oneOfChars (cs:List<char>) : Parser<char> = oneOf (cs |> List.map matchChar)
let oneToNine = oneOfChars ['1'..'9']
let one: Parser<char> = matchChar '1'
let zeroToNine = oneOfChars ['0'..'9']
let digit = zeroToNine
let number: Parser<int> = 
    opt(matchChar '-') ++ (oneOrMore(digit) ^^ charListToInt) ^^ 
    (fun (neg, i) -> match neg with | Some(_) -> -i | _ -> i)

let space = oneOfChars [' '; '\n'; '\t']
let spaces = zeroOrMore space
let numbers = repsep(number, spaces)
let letter = (oneOfChars ['a'..'z']) ||| (oneOfChars ['A'..'Z'])
let underscore = matchChar '_'
let idBody = zeroOrMore(oneOf([letter; digit; underscore]))
let id = letter ++ idBody ^^ (fun (x, xs) -> x :: xs |> charListToString)

let stringLitBody: Parser<string> = zeroOrMore(anyCharBut '"') ^^ charListToString
let stringLit = surroundedBy (matchChar '"') stringLitBody (matchChar '"')

type SExpr = 
  | Number of int
  | Atom of string
  | StrLit of string
  | SList of List<SExpr>
  override self.ToString() = match self with
    | Number n -> string n
    | Atom s ->  s
    | StrLit s -> '"'.ToString() + s + '"'.ToString()
    | SList xs -> "(" + mkString(xs, " ") + ")"

let rec sexpr = oneOf [number ^^ Number; id ^^ Atom; stringLit ^^ StrLit; list ^^ SList]
and list = surroundedByLazy (matchChar '(', lazy(repsep(sexpr, spaces)), matchChar ')')

/// <summary>Blah blah blah <c>mapping</c> Blah blah blah.</summary>
/// <param name="mapping">Yada yada yada</param>
/// <returns>An infinite loop.</returns>
/// <exception cref="System.ArgumentNullException">Thrown occasionally.</exception>