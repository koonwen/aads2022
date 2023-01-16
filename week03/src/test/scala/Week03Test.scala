import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertions.*
import org.scalatest.matchers.should.Matchers._
import com.github.nscala_time.time.Imports.*
import com.github.nscala_time.time.StaticDateTime

import scala.util.Random

class Week03Test extends AnyFunSuite {
  test("memoing speeds up recursive functions with replicated computations") {
    val n = 45
    val start1 = DateTime.now()
    val f1 = fibMemo(BigInt(n))
    val elapsed1: Long = (start1 to DateTime.now()).millis
    println(s"Result1: $f1, elapsed1=$elapsed1 ms")
    val start2 = DateTime.now()
    val f2 = fib(BigInt(n))
    val elapsed2: Long = (start2 to DateTime.now()).millis
    println(s"Result2: $f2, elapsed2=$elapsed2 ms")
    assert(f1 == f2 && elapsed1 * 1000 < elapsed2)
  }

  test("Edit Distance Naive is correct") {
    assert(editDistanceNaive("a", "") == 1)
    assert(editDistanceNaive("abc", "ac") == 1)
    assert(editDistanceNaive("abc", "aac") == 2)
    assert(editDistanceNaive("abcd", "ace") == 3)
    assert(editDistanceNaive("abcdabcd", "abcd") == 4)
    assert(editDistanceNaive("abcdabcdeabcdabcd", "abecd") == 12)
    assert(editDistanceNaive("abcdabcdeabcdabcdabcdabcdeabcdabcd", "abceed") == 28)
    assert(editDistanceNaive("abcdabcdeabcdabcdabcdabcdeabcdabcd", "aaaaa") == 29)
    assert(editDistanceNaive("ab" * 30, "a" * 30) == 30)
  }

  test("Edit Distance Memoed is correct") {
    assert(editDistanceMemo("a", "") == 1)
    assert(editDistanceMemo("abc", "ac") == 1)
    assert(editDistanceMemo("abc", "aac") == 2)
    assert(editDistanceMemo("abcd", "ace") == 3)
    assert(editDistanceMemo("abcdabcd", "abcd") == 4)
    assert(editDistanceMemo("abcdabcdeabcdabcd", "abecd") == 12)
    assert(editDistanceMemo("abcdabcdeabcdabcdabcdabcdeabcdabcd", "abceed") == 28)
    assert(editDistanceMemo("abcdabcdeabcdabcdabcdabcdeabcdabcd", "aaaaa") == 29)
    assert(editDistanceMemo("ab" * 30, "a" * 30) == 30)
  }

  test("Edit Distance DP is correct") {
    assert(editDistanceDP("a", "") == 1)
    assert(editDistanceDP("abc", "ac") == 1)
    assert(editDistanceDP("abc", "aac") == 2)
    assert(editDistanceDP("abcd", "ace") == 3)
    assert(editDistanceDP("abcdabcd", "abcd") == 4)
    assert(editDistanceDP("abcdabcdeabcdabcd", "abecd") == 12)
    assert(editDistanceDP("abcdabcdeabcdabcdabcdabcdeabcdabcd", "abceed") == 28)
    assert(editDistanceDP("abcdabcdeabcdabcdabcdabcdeabcdabcd", "aaaaa") == 29)
    assert(editDistanceDP("ab" * 30, "a" * 30) == 30)
  }

  test("Edit Distance is correct") {
    check {
      val pairs = Gen.listOfN(2, Gen.stringOfN(Random.nextInt(200), Gen.choose('a', 'c')))
      forAllNoShrink(pairs) { p => editDistanceDP(p(0), p(1)) == editDistanceMemo(p(0), p(1)) }
    }(_.withMinSuccessfulTests(200)).status shouldBe Passed
  }

  test("Knapsack test cases"){
    val items = Array(Item(1, 1), Item(2, 6), Item(3, 10), Item(5, 16))
    knapsackNaive(7, items) shouldBe 22
  }

  test("Knapsack comprehensive") {
    check {
      val testCases = for
        l1 <- Gen.listOfN(20, Gen.choose(1, 20))
        l2 <- Gen.listOfN(20, Gen.choose(1, 20))
        l3 = l1 zip l2
        cap <- Gen.choose(50, 500)
      yield (cap, l3.map{ case (m, n) => Item(m, n) }.to(Array))
      forAll(testCases) { case (cap, items) =>
        knapsackDP(cap, items) == knapsackNaive(cap, items)
      }
    }(_.withMinSuccessfulTests(20)).status shouldBe Passed
  }
}
