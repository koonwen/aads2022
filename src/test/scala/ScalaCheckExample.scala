import org.scalacheck._
import Prop._
import Test._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class ScalaCheckExample extends AnyFunSuite:
  test("Reversing a list twice should produce the original list") {
    check {
      forAll { (l: List[String]) => l.reverse.reverse == l }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("The concatenation of two strings should have the second string as a suffix") {
    check {
      forAll {
        (s1: String, s2: String) => (s1 + s2).endsWith(s2)
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("The two Fibonacci methods are equivalent") {
    check {
      forAll(Gen.choose(1, 100)) {
        n => fib.take(n).to(List) == fib2().take(n).to(List)
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }