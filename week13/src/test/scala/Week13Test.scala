import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class Week13Test extends AnyFunSuite:
  test("sample") {
    check {
      forAll { (b: Boolean) => b | !b }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }
