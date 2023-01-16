import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Test._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class Week04Test extends AnyFunSuite {
  test("sample") {
    check {
      forAll { (b: Boolean) => b | !b }
    }(_.withMinSuccessfulTests(20)).status shouldBe Passed
  }
}
