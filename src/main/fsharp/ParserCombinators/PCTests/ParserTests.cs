using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ParserTests {

    public abstract class ParserBaseTest {
        public void testParse<A>(Parsers.Parser<A> parser, string parseMe, A expected) {
            Assert.AreEqual(Parsers.get(parser.Parse(parseMe)), expected);
        }

        public void testParseFailure<A>(Parsers.Parser<A> parser, string parseMe, A expected) {
            var p = parser.Parse(parseMe);
            if (!Parsers.isFailure(p)) Assert.Fail("expected parse failure, but got: " + Parsers.get(p));
        }

        public Microsoft.FSharp.Collections.FSharpList<A> mkFSList<A>(params A[] parsers) {
            return Conversions.toFSharpList<A>(new List<A>(parsers));
        }
    }


    [TestClass]
    public class ParserTests : ParserBaseTest {

        [TestMethod]
        public void TestMatchChar() {
            testParse(Parsers.matchChar('x'), "xxx", 'x');
        }

        [TestMethod]
        public void TestMatchOneOf1() {
            testParse(Parsers.oneOf(mkFSList(Parsers.matchChar('x'), Parsers.matchChar('y'))), "xxx", 'x');
        }

        [TestMethod]
        public void TestMatchOneOf2() {
            testParse(Parsers.oneOf(mkFSList(Parsers.matchChar('x'), Parsers.matchChar('y'))), "yaa", 'y');
        }

        [TestMethod]
        public void TestDigit() {
            testParse(Parsers.digit, "234", '2');
        }

        [TestMethod]
        public void TestZeroOrMore() {
            testParse(Parsers.zeroOrMore(Parsers.one), "1222", mkFSList('1'));
        }

        [TestMethod]
        public void TestZeroOrMore2() {
            testParse(Parsers.zeroOrMore(Parsers.one), "111222", mkFSList('1', '1', '1'));
        }

        [TestMethod]
        public void TestZeroOrMore3() {
            testParse(Parsers.zeroOrMore(Parsers.one), "222", mkFSList<char>());
        }

        [TestMethod]
        public void TestZeroOrMore4() {
            testParse(Parsers.zeroOrMore(Parsers.one), "111", mkFSList('1', '1', '1'));
        }

        [TestMethod]
        public void TestZeroOrMore5() {
            testParse(Parsers.zeroOrMore(Parsers.one), "", mkFSList<char>());
        }

        [TestMethod]
        public void TestOneOrMore() {
            testParse(Parsers.oneOrMore(Parsers.one), "1222", mkFSList('1'));
        }

        [TestMethod]
        public void TestOneOrMore2() {
            testParse(Parsers.oneOrMore(Parsers.one), "111222", mkFSList('1', '1', '1'));
        }

        [TestMethod]
        public void TestOneOrMore3() {
            testParseFailure(Parsers.oneOrMore(Parsers.one), "222", mkFSList<char>());
        }

        [TestMethod]
        public void TestOneOrMore4() {
            testParse(Parsers.oneOrMore(Parsers.one), "111", mkFSList('1', '1', '1'));
        }

        [TestMethod]
        public void TestOneOrMore5() {
            testParseFailure(Parsers.oneOrMore(Parsers.one), "", mkFSList<char>());
        }

        [TestMethod]
        public void TestNumber() {
            testParse(Parsers.number, "1234", 1234);
        }

        [TestMethod]
        public void TestSpaces() {
            testParse(Parsers.spaces, "     ", mkFSList(' ', ' ', ' ', ' ', ' '));
        }

        [TestMethod]
        public void TestNumbers() {
            testParse(Parsers.numbers, "1 2 3 4 5", mkFSList(1, 2, 3, 4, 5));
        }

        [TestMethod]
        public void TestNumbers2() {
            testParse(Parsers.numbers, "123 2   3673      4 5", mkFSList(123, 2, 3673, 4, 5));
        }
    }

    [TestClass]
    public class SExprParserTests : ParserBaseTest {
        [TestMethod]
        public void TestSExprAtom() {
            testParse(Parsers.sexpr, "x", Parsers.SExpr.NewAtom("x"));
        }

        [TestMethod]
        public void TestSExprAtom2() {
            testParse(Parsers.sexpr, "x5", Parsers.SExpr.NewAtom("x5"));
        }

        [TestMethod]
        public void TestSExprAtom3() {
            testParse(Parsers.sexpr, "x_123", Parsers.SExpr.NewAtom("x_123"));
        }

        [TestMethod]
        public void TestSExprAtom4() {
            testParse(Parsers.sexpr, "x__1__z", Parsers.SExpr.NewAtom("x__1__z"));
        }

        [TestMethod]
        public void TestSExprAtom5() {
            testParse(Parsers.sexpr, "x__", Parsers.SExpr.NewAtom("x__"));
        }

        [TestMethod]
        public void TestSExprNumber() {
            testParse(Parsers.sexpr, "5", Parsers.SExpr.NewNumber(5));
        }


        [TestMethod]
        public void TestSExprList() {
            testParse(Parsers.sexpr, "(5)", Parsers.SExpr.NewSList(mkFSList(Parsers.SExpr.NewNumber(5))));
        }

        [TestMethod]
        public void TestSExprList2() {
            testParse(Parsers.sexpr, "(5 x)", Parsers.SExpr.NewSList(
                mkFSList(Parsers.SExpr.NewNumber(5), Parsers.SExpr.NewAtom("x"))));
        }

        [TestMethod]
        public void TestSExprList3() {
            testParse(Parsers.sexpr, "(5 x  x 5)", Parsers.SExpr.NewSList(
                mkFSList(Parsers.SExpr.NewNumber(5), Parsers.SExpr.NewAtom("x"),
                Parsers.SExpr.NewAtom("x"), (Parsers.SExpr.NewNumber(5)))));
        }

        [TestMethod]
        public void TestSExprList4() {
            testParse(Parsers.sexpr, "(5 (x x ) 5)", Parsers.SExpr.NewSList(
                mkFSList(Parsers.SExpr.NewNumber(5),
                Parsers.SExpr.NewSList(mkFSList(Parsers.SExpr.NewAtom("x"), Parsers.SExpr.NewAtom("x"))),
                (Parsers.SExpr.NewNumber(5)))));
        }

        [TestMethod]
        public void TestSExprList5() {
            testParse(Parsers.sexpr, "()", Parsers.SExpr.NewSList(mkFSList<Parsers.SExpr>()));
        }
    }
}