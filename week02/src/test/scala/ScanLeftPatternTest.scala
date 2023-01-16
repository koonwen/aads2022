import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

class ScanLeftPatternTest  extends AnyFunSuite:
  test("MyZip is correct"){
    check {
      val lists = for {
        n <- Gen.choose(50, 100)
        l <- Gen.listOfN(n, Gen.stringOfN(10, Gen.alphaNumChar))
      } yield l
      forAll(lists) { l =>
        myZipWithIndex(l) == l.zipWithIndex
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("ExtractIntegers is correct") {
    check {
      val alphaLists: Gen[String] = for {
        n <- Gen.choose(50, 100)
        l1 <- Gen.listOfN(n, Gen.stringOfN(10, Gen.alphaChar))
        l2 <- Gen.listOfN(n, Gen.stringOfN(Random.nextInt(8), Gen.numChar))
      } yield (l1 zip l2).map(pair => if Random.nextBoolean() then pair._1 else pair._2 + pair._1(0)).mkString("")
      forAll(alphaLists) { s =>
        extractNumbers(s) == s.split("[a-zA-Z]+").to(IndexedSeq).filter(_.nonEmpty).map(_.toInt)
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }