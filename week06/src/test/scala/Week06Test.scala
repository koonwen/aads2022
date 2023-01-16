import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

import com.github.nscala_time.time.Imports.*
import com.github.nscala_time.time.StaticDateTime

class Week06Test extends AnyFunSuite:
  test("Empty treap works correctly") {
    Empty[Int]().isHeap shouldBe true
    Empty[String]().contains("abc") shouldBe false
    new Set[String]().contains("def") shouldBe false
  }

  test("Insert is correct") {
    check {
      val lg = Gen.listOfN(1000, Gen.stringOfN(10, Gen.choose('a', 'z')))
      forAll(lg) { (l: List[String]) =>
        val set = new Set[String]()
        l.foreach(set.insert)
        //println((set.height, 2.5 * Math.log(l.size)/Math.log(2)))
        l.forall(set.contains) && set.isHeap && set.isBST && !set.contains("####")
      }
    }(_.withMinSuccessfulTests(10)).status shouldBe Passed
  }

  test("Delete is correct") {
    check {
      val lg = Gen.listOfN(1000, Gen.stringOfN(5, Gen.choose('a', 'z')))
      forAll(lg) { (l: List[String]) =>
        val set = new Set[String]()
        (l ++ l).foreach(set.insert)
        val idx = Random.nextInt(l.size)
        set.delete(l(idx))
        l.filterNot(_ == l(idx)).forall(set.contains) && !set.contains(l(idx)) && set.size == l.distinct.size - 1
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("Multiple deletes work correctly") {
    check {
      val lg = Gen.listOfN(10000, Gen.stringOfN(5, Gen.choose('a', 'z')))
      forAll(lg) { (l: List[String]) =>
        val set = new Set[String]()
        val ld = l.distinct
        ld.foreach(set.insert)
        val largeSetVerif = set.isHeap && set.isBST && set.size == ld.size
        val twoByTwo = ld.grouped(2).to(List)
        val deletes = twoByTwo.map(_.head)
        val remains = twoByTwo.flatMap(_.drop(1).headOption)
        (deletes ++ deletes).foreach(set.delete)
        remains.forall(set.contains) && deletes.forall(!set.contains(_)) && set.size == remains.size &&
          set.isHeap && set.isBST && !set.isEmpty && largeSetVerif
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("Deleting all elements results in empty set") {
    check {
      val lg = Gen.listOfN(10000, Gen.stringOfN(5, Gen.choose('a', 'z')))
      forAll(lg) { (l: List[String]) =>
        val set = new Set[String]()
        val ld = l.distinct
        ld.foreach(set.insert)
        val largeSetVerif = set.isHeap && set.isBST && set.size == ld.size
        ld.foreach(set.delete)
        set.isEmpty && set.size == 0 && largeSetVerif
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

//  test("performance comparison")  {
//    check {
//      val lg = Gen.listOfN(100000, Gen.stringOfN(5, Gen.choose('a', 'z')))
//      forAll(lg) { (l: List[String]) =>
//        val set = new Set[String]()
//        val bst = collection.mutable.TreeSet[String]()
//        val ld = l.distinct
//        val startTreap = DateTime.now()
//        ld.foreach(set.insert)
//        val elapsedTreap = (startTreap to DateTime.now()).millis
//        println(s"elapsed treap insert: $elapsedTreap")
//        val startBST = DateTime.now()
//        ld.foreach(bst += _)
//        val elapsedBST = (startBST to DateTime.now()).millis
//        println(s"elapsed BST insert: $elapsedBST")
//        val startTreapFind = DateTime.now()
//        val treapFind = ld.forall(set.contains)
//        val elapsedTreapFind = (startTreapFind to DateTime.now()).millis
//        println(s"elapsed treap find: $elapsedTreapFind")
//        val startBSTFind = DateTime.now()
//        val bstFind = ld.forall(bst.contains)
//        val elapsedBSTFind = (startBSTFind to DateTime.now()).millis
//        println(s"elapsed bst find: $elapsedBSTFind")
//        println("========================")
//        set.size == bst.size
//      }
//    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
//  }
