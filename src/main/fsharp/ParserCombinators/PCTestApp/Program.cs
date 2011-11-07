using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Core;
namespace ParsersTestApp
{
    class Program
    {
        public FSharpFunc<T1, FSharpFunc<T2, TResult>> ToFS<T1, T2, TResult>(Func<T1, T2, TResult> f) {
            return null;
        }

        delegate int del(int i);
        static void Main(string[] args)
        {
            // example of calling into the f# code
            Console.WriteLine("hi!: " + Parsers.sexpr.Parse("x"));

            // testing out delegates
            del myDelegate = x => x * x;
            int j = myDelegate(5); //j = 25

            // testing out Func
            Func<int, bool> myFunc = x => x == 5;
            bool result = myFunc(4);
            Console.WriteLine(result);

            // the following doesnt work because 
            // Conversions.CSFuncToFSFunc compiles down to a method that takes the additional int argument
            //Func<int, int> myIntFunc = x => x + 5;
            //Console.WriteLine(Conversions.f2(Conversions.CSFuncToFSFunc<int, int>(myIntFunc), 6));

            // from http://www.voyce.com/index.php/2011/05/09/mixing-it-up-when-f-meets-c/
            var op = FuncConvert.ToFSharpFunc<int, FSharpFunc<int, int>>(aa => 
                FuncConvert.ToFSharpFunc<int, int>(bb => aa + bb)
            );
            var zz = MyModule.apply(op, 1, 2);

            // redo first attempt
            MyModule.f2(FuncConvert.ToFSharpFunc(new Converter<int, int>((x) => { return x + 6; })));

            //c.f2(f.ToFSharpFunc<int,int>(x => x + 6));


            Console.WriteLine(new Lazy<int>().Value);

            var lazyDigit = new Lazy<Parsers.Parser<char>>(() => Parsers.digit);
            var p = Parsers.andThen(Parsers.digit, lazyDigit);
            Console.WriteLine(p.Parse("42aaaa").Get);
            Console.WriteLine(p.Parse("42aaaa"));

            Parsers.ParseResult<String>.NewFailure("hi");

            Console.WriteLine(ParserChecks.x());
        }
    }
}
