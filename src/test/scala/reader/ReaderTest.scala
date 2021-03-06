package reader

import io.Reader

import org.scalacheck.Prop._
import org.scalacheck.Properties

object ReaderTest extends Properties("Reader"){

  // primitivate cases
  testRead("0", 0)
  testRead("1", 1)
  testRead("-1", -1)
  testRead("2147483647", 2147483647)
  testRead("-2147483648", -2147483648)
  testRead("'g'", 'g')
  testRead("'1'", '1')  // this is the character 1, not the number
  testRead("2x2", Symbol("2x2"))
  testRead("hello", 'hello) // unquoted strings are symbols.
  testRead("\"hello\"", "\"hello\"") // quoted strings are strings.
  testRead("\"hello world\"", "\"hello world\"")

  // list cases
  testRead("(hey world)", List('hey, 'world))
  testRead("(\"hey\" world)", List("\"hey\"", 'world))
  testRead("(esi <- -1)", List('esi, '<-, -1))

  // just add a bunch of white space to the last test
  testRead(" (  \"hey\"    world   )   ", List("\"hey\"", 'world))
  // nested list cases
  testRead("(hey (hey world) world)", List('hey, List('hey, 'world), 'world))
  testRead("(hey (hey world) world 1 (1 2))",
    List('hey, List('hey, 'world), 'world, 1, List(1, 2)))

  // error cases
  testReadError("'aa'", "unclosed character literal")
  testReadError("'a", "unclosed character literal")
  testReadError("\"a", "unclosed string literal")
  testReadError("(a", "unclosed list")
  testReadError(")", "unexpected list terminator")

  testRead("""(
  :aint_gonna_happen
  :terminate)""", List(Symbol(":aint_gonna_happen"), Symbol(":terminate")))

  testRead(""";;10
(((eax <- 19)
  (eax <- (print eax))))""", List(List(List('eax, '<-, 19), List('eax, '<-, List('print, 'eax)))))

  testReadWithRest("""(
 ; Round 1
 (x <- y < z)
 (x <- y <= z)
 (x <- y = z))
x
-4
s_
  """, (List(List('x, '<-, 'y, '<, 'z), List('x, '<-, 'y, '<=, 'z), List('x, '<-, 'y, '=, 'z)), "x -4 s_" ))

  testReadWithRest("""(
  ; Test whether spill function correctly ignores things that
  ; shouldn't be spilled. The code itself is nonsense.

  ; The following are labels, not variables, so they should be ignored.
  :x
  (call :x)
)
x
-16
s_""", (List(Symbol(":x"), List('call, Symbol(":x"))),"x -16 s_"))

  testRead("([xlt2 (< x 2)])", List(List('xlt2, List('<, 'x, 2))))
  testRead("((xlt2 (< x 2)))", List(List('xlt2, List('<, 'x, 2))))
  testRead("(let ([x 7]) x)", List('let, List(List('x, 7)), 'x))
  testRead("((let ([x 7]) x))", List(List('let, List(List('x, 7)), 'x)))


  // helper functions
  def read(s:String) = new Reader{}.read(s)
  def readWithRest(s:String) = new Reader{}.readWithRest(s)
  def testRead(s: String, a: Any) = property(s + " => " + a) = secure {
    read(s) == a
  }
  def testReadWithRest(s: String, a: Any) = property(s + " => " + a) = secure{
    readWithRest(s) == a
  }
  def testReadError(s:String,e:String) = property(s + " => " + e) = secure{
    val om = try{ read(s); None } catch { case ex => Some(ex.getMessage) }
    e ?= om.getOrElse(sys.error("expected a read failure, but didnt get one."))
  }
}