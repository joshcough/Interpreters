package util

import org.scalacheck.Prop._
import org.scalacheck.Properties

trait Compare {
  self: Properties =>
  def compare[T](testName: String, actual: => T, expected: => T) = {
    property(testName) = secure {
      val a = actual
      val e = expected
      if (a != e) println("expected: " + e + ", got: " + a)
      a == e
    }
  }
}
