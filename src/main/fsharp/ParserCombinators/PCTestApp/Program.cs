using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PCTestApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("hi!: " + Parsers.sexpr.Parse("x"));
        }
    }
}