module Conversions

let rec mkString<'a> (xs:List<'a>, sep:string): string = match xs with
  | [] -> ""
  | x :: [] -> x.ToString()
  | x :: xs -> x.ToString() + sep + mkString(xs, sep)

let ToList (s:string) : List<char> =
    let rec helper(s:string) : List<char> = 
        if s.Length = 0 then [] else s.[0] :: helper(s.[1..])
    helper(s) |> List.rev

let charListToString (cs: List<char>) : string = cs |> List.map string |> List.fold (+) ""
let charListToInt (cs: List<char>) : int = cs |> charListToString |> int


(**
let toFSharpList<'a> (l:System.Collections.Generic.IEnumerable<'a>) = List.ofSeq(l) 
//let toFSharpList<'a> (l:System.Collections.Generic.List<'a>) = List.ofSeq(l) 
let toEnumerable<'a> (l:List<'a>) = Seq.ofList(l)

// bad. 
let CSFuncToFSFunc<'t, 'u> (f:System.Func<'t, 'u>) = fun t -> f.Invoke(t)

[<System.Runtime.CompilerServices.Extension>]
type public FunctionHelper = 
    [<System.Runtime.CompilerServices.Extension>] 
    static member ToFSharpFunc<'a,'b,'c> (func:System.Func<'a,'b,'c>) = 
        fun x y -> func.Invoke(x,y)
    static member Create<'a,'b,'c> (func:System.Func<'a,'b,'c>) = 
        FunctionHelper.ToFSharpFunc func
**)