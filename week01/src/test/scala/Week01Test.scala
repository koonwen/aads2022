import GTree.{GLeaf, GNode}
import org.scalacheck.*
import Prop.*
import Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class Week01Test extends AnyFunSuite {
  test("fibRecursive should return 0 as the 0th Fibonacci number") {
    fibRecursive(0) shouldBe 0
  }

  test("fibRecursive should return 1 as the first Fibonacci number") {
    fibRecursive(1) shouldBe 1
  }

  test("fibRecursive should return 55 as the 10th Fibonacci number") {
    fibRecursive(10) shouldBe 55
  }

  test("fibTailRecursive should return 0 as the 0th Fibonacci number") {
    fibTailRecursive(0) shouldBe 0
  }

  test("fibTailRecursive should return 1 as the first Fibonacci number") {
    fibTailRecursive(1) shouldBe 1
  }

  test("fibTailRecursive should return 55 as the 10th Fibonacci number") {
    fibTailRecursive(10) shouldBe 55
  }

  test("The two Fibonacci methods should return the same result") {
    check {
      forAll(Gen.choose(1,20)) {
        n => fibRecursive(n) == fibTailRecursive(n)
      }
    }(_.withMinSuccessfulTests(10)).status shouldBe Passed
  }

  test("aMethod and aLambda are equivalent"){
    check {
      forAll { (n: Int) => aMethod(n) == aLambda(n) }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("aLambda and aLambdaWithShortcuts are equivalent"){
    check {
      forAll { (n: Int) => aLambda(n) == aLambdaWithShortcut(n)}
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("Correctness of the tree height method") {
    val t = GNode("a", GNode("b", GLeaf, GNode("c", GLeaf, GLeaf)), GLeaf)
    treeHeight(t) shouldBe 3
  }

  test("Correctness of treeHeight2") {
    val t =
      GNode("a",
        GNode("b", GLeaf, GNode("c", GLeaf, GLeaf)),
        GNode("d", GNode("e", GNode("f", GNode("g", GLeaf, GLeaf), GLeaf), GLeaf), GLeaf)
      )
    treeHeight2(t) shouldBe 5
  }

  test("Tuple properties") {
    (1,"a", 2.0)._3 shouldBe 2.0
    (1, 2, 3) ++ (4, 5) shouldBe (1, 2, 3, 4, 5)
    1 *: (2, 3, 4) shouldBe (1,2,3,4)
    (1, "2", 3, "4").head shouldBe 1
    (1, "2", 3, "4").tail shouldBe ("2", 3, "4")
    (1, 2, 3, 4).take(1) shouldBe Tuple1(1)
    (1, "2", 3, "4").take(2) shouldBe (1, "2")
    (1, "2", 3, "4").drop(2) shouldBe (3, "4")
    (1, "2", 3, "4").drop(3) shouldBe Tuple("4")
    (1, "2", 3, "4").productPrefix shouldBe "Tuple4"
    (1, "2", 3, "4").productArity shouldBe 4
    (10, "20", 30, "40").productElement(2) shouldBe 30
    (10, "20", 30, "40").productIterator.toList shouldBe List(10, "20", 30, "40")
    (10, "20", 30, "40").productElementNames.toList shouldBe List("_1", "_2", "_3", "_4")
  }

  test("List properties") {
    List(1,2,3) ++ List(4,5,6,7) shouldBe List(1,2,3,4,5,6,7)
    3 :: List(4, 5, 6) shouldBe List(3, 4, 5, 6)
    List(10, 20) shouldBe 10::20::Nil
    (List(1, 2, 3, 4) match {
      case a :: _ => a
      case Nil => 0
    }) shouldBe 1
    List(1, 2, 3) :+ 4 shouldBe List(1, 2, 3, 4)
    List(10, 20, 30, 40)(2) shouldBe 30
    List(10, 20, 30, 40).head shouldBe 10
    List(10, 20, 30, 40).size shouldBe 4
    List("abc", "bcd", "cde", "def").find(_.startsWith("b")) shouldBe Some("bcd")
    List("abc", "bcd", "cde", "def").find(_.startsWith("x")) shouldBe None
    List("abc", "bcd", "cde", "def").indexOf("cde") shouldBe 2
    List("abc", "bcd", "cde", "def").indexOf("cdef") shouldBe -1
    List("abc", "bcd", "cde", "def").indexWhere(_.startsWith("b")) shouldBe 1
    List("abc", "bcd", "cde", "def").indexWhere(_.startsWith("x")) shouldBe -1
    an[java.util.NoSuchElementException] should be thrownBy List().head
    List().headOption shouldBe None
    List(10, 20, 30).headOption shouldBe Some(10)
    an[java.lang.UnsupportedOperationException] should be thrownBy List().tail
    List(10, 20, 30).tail shouldBe List(20, 30)
    List(10, 20, 30, 40).filter(_ % 20 == 0) shouldBe List(20, 40)
    List(10, 20, 30).map(_ + 1) shouldBe List(11, 21, 31)
    List(10, 20, 30) zip List("a", "b", "c", "d") shouldBe List((10, "a"), (20, "b"), (30, "c"))
    List(10, 20, 30).zipWithIndex shouldBe List((10, 0), (20, 1), (30, 2))
    List(10, 20, 30, 40) flatMap (n => (1 to 3) map (_ + n)) shouldBe List(11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43)
    List(10, 20, 30, 40).foldLeft(0)(_ + _) shouldBe 100
    List(10, 20, 30, 40).scanLeft(0)(_ + _) shouldBe List(0, 10, 30, 60, 100)
    List(10, 20, 30, 40).forall(_ % 10 == 0) shouldBe true
    List(10, 20, 30, 40).exists(_ % 15 == 0) shouldBe true
    List(10, 20, 30, 40).take(2) shouldBe List(10, 20)
    List(10, 20, 30, 40).takeWhile(_ < 25) shouldBe List(10, 20)
    List(10, 20, 30, 40).drop(2) shouldBe List(30, 40)
    List(10, 20, 30, 40).dropWhile(_ < 25) shouldBe List(30, 40)
  }

  test("Option properties") {
    Option(10) shouldBe Some(10)
    Option(null) shouldBe None
    Option(10).filter(_ % 2 == 0) shouldBe Some(10)
    Option(10).filter(_ % 2 == 1) shouldBe None
    Option(10).map(_ + 1) shouldBe Option(11)
    None.map((_:Int) + 1) shouldBe None
    Option(10).get shouldBe 10
    Option(10).head shouldBe 10
    an [java.util.NoSuchElementException] should be thrownBy None.get // same for head
    Option(10).getOrElse(20) shouldBe 10
    None.getOrElse(20) shouldBe 20
    Option(10) orElse Option(20) shouldBe Some(10)
    None orElse Option(20) shouldBe Some(20)
    Option(20).to(List) shouldBe List(20)
    None.to(List) shouldBe Nil
  }

  test("Immutable Array properties") {
    IArray(10, 20, 30, 40)(2) shouldBe 30
    IArray(10, 20, 30, 40).updated(2, 100) shouldBe IArray(10, 20, 100, 40) // new array is returned
    // find, indexOf, indexWhere, forall, exists, map, zip, flatMap,
    // foldLeft, scanLeft, take, takeWhile, drop, dropWhile similar to List
  }

  test("Immutable Set properties") {
    Set(2, 3, 4, 5) shouldBe Set(5, 4, 3, 2)
    Set(2, 3, 4, 5) + 6 shouldBe Set(2, 3, 4, 5, 6)
    Set(2, 3, 4, 5) + 4 shouldBe Set(2, 3, 4, 5)
    Set(2, 3, 4, 5) - 4 shouldBe Set(2, 3, 5)
    Set(2, 3, 4, 5) - 6 shouldBe Set(2, 3, 4, 5)
    Set(2, 3, 4, 5)(3) shouldBe true // test membership
    Set(2, 3, 4, 5)(10) shouldBe false
    Set(2, 3, 4, 5) subsetOf Set(1, 2, 3, 4, 5, 6) shouldBe true
    Set(1, 2, 3).subsets.to(Set) shouldBe Set(Set(), Set(1), Set(2), Set(3), Set(1,2), Set(1,3), Set(2,3), Set(1,2,3))
    Option(10).to(Set) shouldBe Set(10)
    None.to(Set) shouldBe Set()
    Set(1,2,3).to(List).sorted shouldBe List(1,2,3)
    List(3,2,1,3).to(Set) shouldBe Set(1, 2, 3)
    // filter, map, forall, exists, flatMap, size, foldLeft, scanLeft similar to list
  }

  test("Immutable Map properties") { // Maps can be seen as sets of pairs in many circumstances
    ("a" -> 10) shouldBe ("a", 10)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40) shouldBe Map("d" -> 40, "c" -> 30, "b" -> 20, "a" -> 10)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40)("b") shouldBe 20
    a [NoSuchElementException] should be thrownBy Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40)("e")
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).get("e") shouldBe None
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).getOrElse("e", 100) shouldBe 100
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).get("a") shouldBe Some(10)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).getOrElse("a", 100) shouldBe 10
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).size shouldBe 4
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40) + ("e" -> 50) shouldBe
      Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40, "e" -> 50)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40) + ("c" -> 50) shouldBe
      Map("a" -> 10, "b" -> 20, "c" -> 50, "d" -> 40)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40) ++ List("a" -> 50, "e" -> 100) shouldBe
      Map("a" -> 50, "b" -> 20, "c" -> 30, "d" -> 40, "e" -> 100)
    Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).to(Set) shouldBe Set("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40)
    Set("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40).to(Map) shouldBe Map("a" -> 10, "b" -> 20, "c" -> 30, "d" -> 40)
    // filter, map, forall, exists, flatMap, foldLeft, scanLeft converts the map to a set of pairs, and the result back to Map
  }
}
