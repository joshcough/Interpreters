package util

import org.scalacheck.Prop._
import org.scalacheck.Properties

trait Compare { self: Properties =>
  def compare[T](testName: String, actual: => T, expected: => T) = {
    property(testName) = secure {
      val a = actual.toString
      val e = expected.toString
      // println("expected: " + e + ", got: " + a)
      if (a != e)
        println("expected: " + e + ", got: " + a) //"(with classes [" + e.getClass + ", " + a.getClass + "])")
      a == e
    }
  }
  def compareR[T](name:String, actual: Either[String, T], expected:T) =
    compare(name, actual, Right(expected))
}
