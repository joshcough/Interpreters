module Parsers
#light

type ParseResult<'a> = 
  | Success of 'a * string
  | Failure of string

let get pr = match pr with
    | Success(a, _) -> a
    | _ -> failwith("cant get from a failure")

let isFailure pr =  match pr with 
    | Failure(_) -> true
    | _ -> false

let getFailureMessage pr =  match pr with
    | Failure(m) -> m
    | _ -> failwith("not a failure") 

type Parser<'a> =
    abstract member Parse: string -> ParseResult<'a>

let inline (^^) (p:Parser<'a>)(f: 'a -> 'b) =  { 
    new Parser<'b> with member this.Parse(s) =  match p.Parse(s) with
        | Success (a, rest) -> Success(f(a), rest)
        | Failure (message) -> Failure(message)
}                 
let inline (|||) (l:Parser<'a>)(r:Parser<'a>) = {
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
let inline (++) (l:Parser<'a>)(r:Parser<'b>) = andThen(l, lazy(r)) 
let inline (+++) (l:Parser<'a>)(r:Lazy<Parser<'b>>) = andThen(l,r) 
let inline (^^^) (p:Parser<'a>)(v:Lazy<'b>) = {
    new Parser<'b> with member this.Parse(s) = match p.Parse(s) with
        | Success (_, rest) -> Success(v.Force(), rest)
        | Failure (message) -> Failure(message)
}

let opt<'a>(p:Parser<'a>) = {
    new Parser<Option<'a>> with member this.Parse(s) = match p.Parse(s) with
        | Success (v, rest) -> Success(Some(v), rest)
        | Failure (message) -> Success(None, s)
}
let never<'a> () = { new Parser<'a> with member p.Parse(s) = Failure("never") }
let rec oneOf<'a> (parsers: List<Parser<'a>>) : Parser<'a> = 
    match parsers with
    | p :: ps -> p ||| oneOf<'a>(ps)
    | _ -> never()
let matchWord (findMe:string) = {
    new Parser<string> with member p.Parse(s) = 
        if findMe.Length <= s.Length && s.Substring(0, findMe.Length).Equals(findMe) 
        then Success(findMe, s.Substring(findMe.Length)) 
        else Failure("didn't find: " + findMe)
}
let matchChar (c:char) = matchWord(string c) ^^ (fun s -> s.[0])
let emptyString = matchWord("")
let rec zeroOrMore<'a> (p:Parser<'a>): Parser<List<'a>> =
    (p +++ lazy(zeroOrMore<'a>(p)) ^^ List.Cons) ||| (emptyString ^^^ lazy([]))
let oneOrMore<'a> (p:Parser<'a>): Parser<List<'a>> = p ++ zeroOrMore<'a>(p) ^^ List.Cons
let repsep<'a, 'b> (pa:Parser<'a>, pb:Parser<'b>) : Parser<List<'a>> =
    zeroOrMore(pa ++ pb ^^ fst) ++ (opt(pa) ^^ Option.toList) ^^ (fun (l, r) -> l @ r)
let oneOfChars (cs:List<char>) : Parser<char> = oneOf (cs |> List.map matchChar)
let oneToNine = oneOfChars ['1'..'9']
let one: Parser<char> = matchChar '1'
let zeroToNine = oneOfChars ['0'..'9']
let digit: Parser<char> = zeroToNine
let charListToString (cs: List<char>) : string = cs |> List.map string |> List.reduce (+)
let charListToInt (cs: List<char>) : int = cs |> charListToString |> int
let number: Parser<int> = oneOrMore(digit) ^^ charListToInt
let space = oneOfChars [' '; '\n'; '\t']
let spaces = zeroOrMore space
let numbers = repsep(number, spaces)
let letter = (oneOfChars ['a'..'z']) ||| (oneOfChars ['a'..'z'])
let underscore = matchChar '_'
let idBody = zeroOrMore(oneOf([letter; digit; underscore]))
let id = letter ++ idBody ^^ (fun (x, xs) -> x :: xs |> charListToString)

type SExpr = 
  | Number of int
  | Atom of string
  | SList of List<SExpr>

let rec sexpr = oneOf [number ^^ Number; id ^^ Atom; list ^^ SList]
and listStart<'a> = matchChar('(') ^^^ lazy([])
and listEnd<'a> = matchChar(')') ^^^ lazy([])
and list = listStart +++ lazy(repsep(sexpr, spaces)) +++ lazy(listEnd) ^^ (fun ((_, l), _) -> l)

/// <summary>Blah blah blah <c>mapping</c> Blah blah blah.</summary>
/// <param name="mapping">Yada yada yada</param>
/// <returns>An infinite loop.</returns>
/// <exception cref="System.ArgumentNullException">Thrown occasionally.</exception>