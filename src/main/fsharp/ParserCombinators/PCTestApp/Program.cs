﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Core;
using c=Conversions;
using f=FuncConvert;

namespace ParsersTestApp
{
    class Program
    {
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
            var zz = Conversions.apply(op, 1, 2);

            // redo first attempt
            c.f2(FuncConvert.ToFSharpFunc(new Converter<int, int>((x) => { return x + 6; })));

            c.f2(f.ToFSharpFunc<int,int>(x => x + 6));
        }
    }
}
