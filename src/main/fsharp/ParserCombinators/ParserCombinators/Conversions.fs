module Conversions

//let toFSharpList<'a> (l:System.Collections.Generic.IEnumerable<'a>) = List.ofSeq(l) 
let toFSharpList<'a> (l:System.Collections.Generic.List<'a>) = List.ofSeq(l) 
let toEnumerable<'a> (l:List<'a>) = Seq.ofList(l)

// bad. 
let CSFuncToFSFunc<'t, 'u> (f:System.Func<'t, 'u>) = fun t -> f.Invoke(t)

//let CSFuncToFSFunc<'t, 'u> (f:System.Func<'t, 'u>, t:int) = f.Invoke(t)

// just testing things.
let f = fun x -> x + 7
let f2 : (int -> int) -> int = fun x -> (x 2)


// from http://www.voyce.com/index.php/2011/05/09/mixing-it-up-when-f-meets-c/
let apply op a b = op a b