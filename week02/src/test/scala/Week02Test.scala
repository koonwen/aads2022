import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random
import com.github.nscala_time.time.Imports.*
import com.github.nscala_time.time.StaticDateTime

class Week02Test extends AnyFunSuite {
  val tree: Map[String, List[String]] = Map(
    "bernard" -> List("frank", "eugene"),
    "george" -> List("patty", "olga"),
    "ian" -> List("stan"),
    "frank" -> List("nick", "mary"),
    "eugene" -> List("larry"),
    "joseph" -> List("verna", "ursula", "tom"),
    "harry" -> List("rob", "quine"),
    "david" -> List("karl", "joseph", "ian"),
    "charles" -> List("harry", "george"),
    "adam" -> List("david", "charles", "bernard"),
    "karl" -> List("zack", "yara", "xena", "will")
  )

  test("The first two subtreeNodes methods return the same result") {
    check {
      forAll(Gen.oneOf(tree.keys)) { e => subtreeNodes1(tree, e).toSet == subtreeNodes2(tree, e).toSet  }
    }(_.withMinSuccessfulTests(10)).status shouldBe Passed
  }

  test("The first and third subtreeNodes methods return the same result") {
    check {
      forAll(Gen.oneOf(tree.keys)) { e => subtreeNodes1(tree, e).toSet == subtreeNodes3(tree, e).toSet  }
    }(_.withMinSuccessfulTests(10)).status shouldBe Passed
  }

  val weightTree: Map[String, Int] = Map(
    "patty" -> 35,
    "verna" -> 20,
    "george" -> 60,
    "quine" -> 30,
    "frank" -> 70,
    "larry" -> 50,
    "eugene" -> 80,
    "stan" -> 30,
    "harry" -> 65,
    "nick" -> 40,
    "bernard" -> 90,
    "david" -> 70, "charles" -> 80,
    "ursula" -> 25,
    "ian" -> 60,
    "tom" -> 25,
    "mary" -> 45,
    "xena" -> 15,
    "joseph" -> 55,
    "yara" -> 15,
    "adam" -> 100,
    "rob" -> 35,
    "karl" -> 50,
    "zack" -> 10,
    "will" -> 20,
    "olga" -> 40
  )

  test("teamWeight returns the correct result") {
    List(subtreeNodes1, subtreeNodes2, subtreeNodes3).foreach { f =>
      teamWeight("charles", tree, weightTree, f) shouldBe 345
    }
  }

  test("bubbleSort should sort the array") {
    // val complexitySamples = collection.mutable.TreeMap.empty[Int, Long]
    check {
      forAll(Gen.choose(1000, 10000)) { // 10000, 100000 for better sampling
        n =>
          val sample = List.fill(n)(Random.nextInt)
          // println(sample.size)
          val arr = sample.to(Array)
          // val start = DateTime.now()
          bubbleSort(arr)
          // val elapsed = (start to DateTime.now()).millis
          // complexitySamples += (n -> elapsed)
          // println((complexitySamples.size, n, elapsed))
          arr.to(List) == sample.sorted
      }
    }(_.withMinSuccessfulTests(10)).status shouldBe Passed
     // println(complexitySamples.keysIterator.map(_.toDouble/1000).mkString("[",",","]"))
     // println(complexitySamples.keysIterator.map(complexitySamples(_).toDouble/1000).mkString("[",",","]")) 
     // println(complexitySamples.to(List))
  }
  // Some samples:
  // [12.232,18.909,19.667,24.591,24.882,45.036,47.609,59.605,61.029,93.561]
  // [0.714,4.751,2.022,3.264,2.941,11.968,11.402,19.386,19.835,49.575]
  // ((12232,714), (18909,4751), (19667,2022), (24591,3264), (24882,2941), (45036,11968), (47609,11402), (59605,19386), (61029,19835), (93561,49575))

  // Generate complex but relevant samples for testing
  // =================================================

  // Generate trees for subtreeNodes

  // Generate a random number from a non-uniform probability distribution. This number is used as
  // the number of descendants for the current inner node.
  def randomChildren: Int =
    val fanOut = 10
    val degreeRatio = 1.2
    fanOut - Math.pow(Random.nextInt(Math.pow(fanOut, degreeRatio).round.toInt),1/degreeRatio).round.toInt

  // Generate a random tree from a list of strings representing node names.
  def genTree(l: List[String]): Map[String, List[String]] =
    Iterator.iterate((l.tail, Map.empty[String, List[String]], List.empty[String], randomChildren, l.head)) {
      case (q::qs, ts, rs, k, root) if rs.size > k && k > 0 => (qs, ts + (q -> rs.take(k)), q::rs.drop(k), randomChildren, root )
      case (q::qs, ts, rs, k, root) => (qs, ts, q::rs, randomChildren, root )
      case (Nil, ts, rs @ _::_, _, root) => (Nil, ts + (root -> rs), Nil, randomChildren, root)
      case p @ (Nil, ts, Nil, _, _) => p
    }.dropWhile(p => p._1.nonEmpty || p._3.nonEmpty).next._2

  // Generate "<parent> -> <child>" text lines that can be used to create a .dot file that can be rendered bu
  def subtreeGenDot(t: Map[String, List[String]]): Unit =
    t.toSeq.foreach{case (n,l) => l.foreach(m => println(s"x$n -> x$m"))}

  test("subtreeNodes must work correctly for random trees"){
    check {
      val lists = for {
        n <- Gen.choose(50,100)
        l <- Gen.listOfN(n, Gen.stringOfN(10, Gen.alphaNumChar))
      } yield l
      forAll(lists){ l =>
        val t = genTree(l)
        val root = getRoot(t)
        if root.nonEmpty && root.get == l.head then ()
        else throw new RuntimeException("Generated map is not a valid tree")
        subtreeGenDot(t)
        println(s"height of tree above: ${subtreeNodeHeight(t, l.head)}${System.lineSeparator}")
        val testRoot = l(Random.nextInt(l.size))
        val result1 = subtreeNodes1(t, testRoot).to(Set)
        val result2 = subtreeNodes2(t, testRoot).to(Set)
        val result3 = subtreeNodes3(t, testRoot).to(Set)
        result1 == result2 && result2 == result3
      }
    }(_.withMinSuccessfulTests(100)).status shouldBe Passed
  }

  test("Consensus must be correct") {
    val strands = List(
      "ATCCAGCT",
      "GGGCAACT",
      "ATGGATCT",
      "AAGCAACC",
      "TTGGAACT",
      "ATGCCATT",
      "ATGGCACT"
    )
    consensus(strands) shouldBe "ATGCAACT"
  }
}
