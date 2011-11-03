using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.FSharp.Collections;

namespace PCTests
{
    [TestClass]
    public class ConversionsTests
    {
        [TestMethod]
        public void TestMethod1()
        {
            //MyModule.f3(Conversions.FunctionHelper.Create<int, int, int>((x, y) => x + y));
            MyModule.f3_cs((x, y) => x + y);

            // convert from C# List to F# List
            List<int> ints = new List<int> { 1, 2, 3 };
            Console.WriteLine(ListModule.OfSeq(ints));
            Microsoft.FSharp.Collections.FSharpList<int> xs = ListModule.OfSeq(ints);
            
            // Convert from C# Dict to F# List
            Dictionary<string, int> dict = new Dictionary<string, int>();
            dict.Add("cat", 2);
            dict.Add("dog", 1);
            dict.Add("llama", 0);
            dict.Add("iguana", -1);
            Console.WriteLine(ListModule.OfSeq(dict));
            Microsoft.FSharp.Collections.FSharpList<KeyValuePair<string, int>> ys = ListModule.OfSeq(dict);

            // convert from F# List to C# IEnumberable
            IEnumerable<int> xs2 = SeqModule.OfList(xs);
        }
    }
}
