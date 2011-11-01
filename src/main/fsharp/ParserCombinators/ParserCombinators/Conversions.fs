module Conversions

//let toFSharpList<'a> (l:System.Collections.Generic.IEnumerable<'a>) = List.ofSeq(l) 
let toFSharpList<'a> (l:System.Collections.Generic.List<'a>) = List.ofSeq(l) 
let toEnumerable<'a> (l:List<'a>) = Seq.ofList(l)