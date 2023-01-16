import org.scalatest._
import funsuite._
import matchers._
import prop._

class ScalaTestExample extends AnyFunSuite with should.Matchers:
    test("Simple addition check") {
      (2 + 3) shouldBe 5
    }

    test("Taking 0 elements from a list produces the empty list") {
      assert {
        List(1,2,3).take(0).isEmpty
      }
    }

    test("The two Fibonacci methods are equivalent") {
      fib.take(20).to(List) shouldBe fib2().take(20).to(List)
    }

