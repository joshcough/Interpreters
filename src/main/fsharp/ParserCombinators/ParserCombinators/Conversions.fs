module Conversions

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