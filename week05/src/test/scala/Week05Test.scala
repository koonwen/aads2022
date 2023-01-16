import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random
import com.github.nscala_time.time.Imports.*
import com.github.nscala_time.time.StaticDateTime

class Week05Test extends AnyFunSuite:
  test("sample") {
    check {
      forAll { (b: Boolean) => b | !b }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("IList correctness") {
    assert(IList(2, 3, 4) == 2 :::: 3 :::: 4 :::: INil) // (((INil.::::(4)).::::(3)).::::(2))
    assert(IList() == INil)                             // a.meth(b).m2(c)   === a meth b m2 c
    assert(IList(2,3,4,5,6).reverse == IList(6,5,4,3,2))//   2 + 3    === 2.+(3)
    assert(IList(2,3,4) ++++ IList(5,6,7) == IList(2,3,4,5,6,7))
    assert(IList(4,5,6).head == 4)
    intercept[NoSuchElementException](IList().head)
    assert(IList(4, 5, 6).headOption.contains(4))
    assert(IList().headOption.isEmpty)
    assert(IList(4,5,6).tail == 5 :::: 6 :::: INil)
    intercept[NoSuchElementException](IList().tail)
  }

  test("IStack correctness") {
    assert(IStack(1,2,3,4).push(5) == IStack(5,1,2,3,4))
    assert(IStack(1,2,3,4).pop == (1, IStack(2,3,4)))
    intercept[NoSuchElementException](IStack(1).pop._2.pop)
    intercept[NoSuchElementException](IStack(1).pop._2.peek)
    assert(IStack(1,2,3,4).push(5).peek == 5)
    assert(!IStack().push(1).push(2).isEmpty)
    assert(IStack().push(1).pop._2.isEmpty)
  }

  test("IQueue correctness") {
    assert(IQueue(1,2).enqueue(3).enqueue(4).dequeue._1 == 2)
    assert(IQueue(1,2).enqueue(3).enqueue(4).peek == 2)
    assert(IQueue(1,2).enqueue(3).enqueue(4).dequeue._2.dequeue._2.dequeue._1 == 3)
    assert(IQueue().enqueue(1).dequeue._2.isEmpty)
    assert(!IQueue().enqueue(1).isEmpty)
    intercept[NoSuchElementException](IQueue().enqueue(1).dequeue._2.dequeue)
    intercept[NoSuchElementException](IQueue().enqueue(1).dequeue._2.peek)
    assert(IQueue(1).dequeueOption match {
      case Some((v, q)) => v == 1 && q.isEmpty
      case _ => false
    })
    assert(IQueue(1).peekOption.contains(1))
  }

  test("DelayedVal is correct") {
    val v1 = DelayedVal[Int]()
    assert(v1.value.isEmpty)
    v1.update(3)
    assert(v1.value.nonEmpty)
    intercept[IllegalStateException]{ v1.update(0) }
    assert(v1.tryUpdate(10) == (3, false))
    assert(v1.value == Option(3))
    val v2 = DelayedVal[String]()
    assert(v2.tryUpdate("abc") == ("abc", true))
    assert(v2.value == Option("abc"))
  }

  def isLeftist[T]: ITree[T] => Boolean = {
    case ILeaf() => true
    case INode(_, left, right, _, _, _) => left.depth >= right.depth
  }


  test("IHeap is correct") {
    check {
      val l = Gen.listOfN(3, Gen.choose(1, 1000000000))
      forAll(l){ (l: List[Int]) =>
        l.length > 1 ==> {
          val heap1 = l.foldLeft(IHeap.empty[Int]){ case (h, i) => h.insert(i) }
          val lsort = l.sorted
          val min = lsort.head
          val secondMin = lsort(1)
          val heap2 = heap1.withoutMin
          val heap3 = heap2.insert(Int.MinValue)
          heap1.peekMin.get == min && heap2.peekMin.get == secondMin && heap3.peekMin.get == Int.MinValue &&
            heap1.tree.size == l.length &&
            isLeftist(heap1.tree) && isLeftist(heap2.tree) && isLeftist(heap3.tree)
        }
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("immutableSort is correct") {
    check {
      val intList = Gen.listOfN(10000, Gen.choose(1,1000000))
      forAll(intList) { (l: List[Int]) => immutableSort(IList(l: _*)) == IList(l.sorted : _*) }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("mutableSort is correct"){
    check {
      val intList = Gen.listOfN(10000, Gen.choose(1,1000000))
      forAllNoShrink(intList) { (l: List[Int]) =>
        val a = l.to(Array)
        mutableSort(a)
        (1 until a.length).forall(i => a(i - 1) <= a(i))
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("performance comparison"){
    check {
      val intList = Gen.listOfN(1000000, Gen.choose(1,1000000))
      forAllNoShrink(intList) { (l: List[Int]) =>
        System.gc()
        val startImmutable = DateTime.now()
        val immutableResult = immutableSort(IList(l: _*))
        val elapsedImmutable = (startImmutable to DateTime.now()).millis
        val a = l.to(Array)
        System.gc()
        Thread.sleep(1000)
        val startMutable = DateTime.now()
        mutableSort(a)
        val elapsedMutable = (startMutable to DateTime.now()).millis
        val startPlatformSorted = DateTime.now()
        val sorted = l.sorted
        val elapsedPlatformSorted = (startPlatformSorted to DateTime.now()).millis
        println(s"size = ${l.length}, platform sort time = $elapsedPlatformSorted, mutable sort time = $elapsedMutable, " +
          s"immutable sort time = $elapsedImmutable, immutable/mutable ratio = ${elapsedImmutable.toDouble / elapsedMutable}, " +
          s"mutable/platform ratio = ${elapsedMutable.toDouble / elapsedPlatformSorted}")
        a.foldLeft((true, immutableResult)) { case ((b, l), e) => (b && l.head == e, l.tail)}._1
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("Hidden mutability"){
    check {
      val intList = Gen.listOfN(1000000, Gen.choose(1,1000000))
      forAllNoShrink(intList) { (l: List[Int]) =>
        System.gc()
        Thread.sleep(2000)
        val startHidden = DateTime.now()
        val sorted1 = IList(l: _*).sort
        val elapsedHidden = (startHidden to DateTime.now()).millis
        val startImmutable = DateTime.now()
        val sorted2 = immutableSort(IList(l: _*))
        val elapsedImmutable = (startImmutable to DateTime.now()).millis
        println(s"size = ${l.length}, hidden mutability sort time = $elapsedHidden, " +
          s"immutable sort time = $elapsedImmutable, immutable/hidden ratio = ${elapsedImmutable.toDouble / elapsedHidden}")
        sorted1 == sorted2
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }
