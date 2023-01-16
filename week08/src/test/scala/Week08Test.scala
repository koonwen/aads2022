import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import collection.immutable.ArraySeq
import scala.util.{Random, Try, Success, Failure}

class Week08Test extends AnyFunSuite:

  implicit def dist(p0: ArraySeq[Double], p1: ArraySeq[Double]): Double =
    Math.sqrt(p0.zip(p1).map{ case (c0, c1) => (c0 - c1) * (c0 - c1)}.sum)

  implicit def largestValue: Double = Double.MaxValue

  implicit def diff(a: Double, b: Double): Double = a - b

  test("Building a k-d tree with 1 node works correctly"){
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 2 nodes when child goes to right works correctly") {
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d), ArraySeq(1d,-1d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 2 nodes when child goes to left works correctly") {
    val t = KdTree[Double](2, Array(ArraySeq(1d,-1d), ArraySeq(0d,0d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 3 nodes -- balanced -- works correctly") {
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d), ArraySeq(1d,-1d), ArraySeq(-1d, 1d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 3 nodes in reverse order -- balanced ") {
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d), ArraySeq(-1d, 1d), ArraySeq(1d, -1d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 7 nodes -- balanced -- works correctly") {
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d), ArraySeq(1d,-1d), ArraySeq(-1d, 1d), ArraySeq(2d,-2d), ArraySeq(-2d, 2d), ArraySeq(3d,-3d), ArraySeq(-3d, 3d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree with 7 nodes in reverse order -- balanced ") {
    val t = KdTree[Double](2, Array(ArraySeq(0d,0d), ArraySeq(-1d, 1d), ArraySeq(1d, -1d), ArraySeq(-2d,2d), ArraySeq(2d, -2d), ArraySeq(-3d,3d), ArraySeq(3d, -3d)))
    t.isKdTree shouldBe true
  }
  
  test("Building a k-d tree works correctly for any number of nodes") {
    val pointsG = for {
      xs <- Gen.listOfN(20, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(20, Gen.choose(-100000d, 100000d))
    } yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        t.isKdTree && t.size == coords.length
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("Search works correctly for points known to be in the tree") {
    val pointsG = for {
      xs <- Gen.listOfN(50, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(50, Gen.choose(-100000d, 100000d))
    } yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val point = coords(Random.nextInt(coords.length))
        t.search(point).exists(_.coords == point)
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("Search works correctly for points known not to be in the tree") {
    val pointsG = for {
      xs <- Gen.listOfN(50, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(50, Gen.choose(-100000d, 100000d))
    } yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val point = ArraySeq(Random.nextInt.toDouble, Random.nextInt.toDouble)
        (! t.search(point).exists(_.coords == point))
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("Insert works correctly"){
    val pointsG = for {
      xs <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
    } yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val point = ArraySeq(Random.nextInt.toDouble, Random.nextInt.toDouble)
        val newPointNotYetin = ! t.search(point).exists(_.coords == point)
        t.insert(point)
        t.search(point).exists(_.coords == point) && newPointNotYetin
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("findMin works correctly"){
    val pointsG = for {
      xs <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
    } yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val idx = Random.nextInt(2)
        val point = coords.minBy(_(idx))
        t.findMin(idx).exists(_.coords == point)
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("remove works correctly") {
    val pointsG = for
      xs <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
    yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val point = coords(Random.nextInt(coords.length))
        t.remove(point)
        t.isKdTree && t.size == coords.length - 1
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }

  test("NN of Leaf is None"){
    val t = KdTree[Double](2, Array.empty[ArraySeq[Double]])
    t.nearestNeighbor(ArraySeq(0d, 0d)) shouldBe None
  }

  test("NN of single node is that node's coords"){
    val t = KdTree[Double](2, Array(ArraySeq(1d, -1d)))
    t.nearestNeighbor(ArraySeq(0d, 0d)) shouldBe Some(ArraySeq(1d, -1d))
  }

  test("NN of double node works correctly for target obviously close to one node"){
    val t = KdTree[Double](2, Array(ArraySeq(1d, -1d), ArraySeq(100d, 100d)))
    t.nearestNeighbor(ArraySeq(101d, 101d)) shouldBe Some(ArraySeq(100d, 100d))
  }

  test("NN of double node works correctly for target not obviously close to node") {
    val t = KdTree[Double](2, Array(ArraySeq(0d, 0d), ArraySeq(-1d, 5d)))
    t.nearestNeighbor(ArraySeq(1d, 5d)) shouldBe Some(ArraySeq(-1d, 5d))
  }

  test("NN of triple node works correctly for target not obviously close to node") {
    val t = KdTree[Double](2, Array(ArraySeq(-1d, 4d), ArraySeq(0d, 0d), ArraySeq(-1d, 5d)))
    t.nearestNeighbor(ArraySeq(1d, 5d)) shouldBe Some(ArraySeq(-1d, 5d))
  }

  test("NN works correctly for random trees") {
    val pointsG = for
      xs <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
      ys <- Gen.listOfN(100, Gen.choose(-100000d, 100000d))
    yield xs.zip(ys).map{ case (x, y) => ArraySeq(x, y)}.toArray
    check {
      forAllNoShrink(pointsG) { (coords: Array[ArraySeq[Double]]) =>
        val t = KdTree[Double](2, coords)
        val point = coords(Random.nextInt(coords.length))
        val nn = coords.map(p => (p, dist(p, point))).minBy(_._2)._1
        t.nearestNeighbor(point).contains(nn)
      }
    }(_.withMinSuccessfulTests(1000)).status shouldBe Passed
  }





