package util

import org.scalatest.FunSuite
import java.io.File
import io.FileHelper._

trait TestHelpers extends FunSuite {
  implicit def pimpedString(s:String) = new {
    def clean = s.stripMargin.trim
  }

  def verboseAssert(code:String, actual: String, expected: String) {
    if (actual.clean != expected.clean) {
      println("code:\n" + code.clean)
      println("actual:\n" + actual.clean)
      println("expected:\n" + expected.clean)
    }
    assert(actual.clean === expected.clean)
  }
}

