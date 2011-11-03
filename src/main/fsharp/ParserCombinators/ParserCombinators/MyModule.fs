// just a place to stick some generic/random functions for testing purposes
module MyModule

// just testing things.
let f = fun x -> x + 7
let f2 : (int -> int) -> int = fun x -> (x 2)
let f3 (func : int -> int -> int) : int = func 6 7
let f3_cs (func : System.Func<int,int,int>) : int = f3 (fun x -> (fun y -> func.Invoke(x, y)))

// from http://www.voyce.com/index.php/2011/05/09/mixing-it-up-when-f-meets-c/
let apply op a b = op a b
