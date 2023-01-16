import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import scala.io.Source

class Week07Test extends AnyFunSuite:
  test("insert a 1-letter word") {
    val stringSet = StringSet()
    stringSet.insert("a")
    stringSet.size shouldBe 1
    stringSet.depth shouldBe 1
    stringSet.contains("a") shouldBe true
    stringSet.contains("b") shouldBe false
    stringSet.contains("ab") shouldBe false
  }

  test("insert 2 1-letter words") {
    val stringSet = StringSet()
    stringSet.insert("a")
    stringSet.insert("b")
    stringSet.size shouldBe 2
    stringSet.depth shouldBe 1
    stringSet.contains("a") shouldBe true
    stringSet.contains("b") shouldBe true
    stringSet.contains("c") shouldBe false
    stringSet.contains("ab") shouldBe false
    stringSet.contains("ba") shouldBe false
  }

  test("insert 1 letter word twice") {
    val stringSet = StringSet()
    stringSet.insert("a")
    stringSet.insert("b")
    stringSet.insert("a")
    stringSet.size shouldBe 2
    stringSet.depth shouldBe 1
    stringSet.contains("a") shouldBe true
    stringSet.contains("b") shouldBe true
    stringSet.contains("c") shouldBe false
    stringSet.contains("ab") shouldBe false
    stringSet.contains("ba") shouldBe false
  }

  test("insert a prefix first") {
    val stringSet = StringSet()
    stringSet.insert("a")
    stringSet.insert("art")
    stringSet.size shouldBe 2
    stringSet.depth shouldBe 3
    stringSet.contains("a") shouldBe true
    stringSet.contains("art") shouldBe true
    stringSet.contains("ar") shouldBe false
    stringSet.contains("artsy") shouldBe false
  }

  test("insert multiple words") {
    import scala.util.Random
    val words = List("a", "art", "artsy", "able", "adept", "adorn", "adore", "adoration", "bed",
      "beds", "bedroom", "bedrooms", "bee", "beef", "before"
    )
    val stringSet = StringSet()
    Random.shuffle(words ++ words).foreach(stringSet.insert)
    stringSet.size shouldBe words.size
    stringSet.depth shouldBe words.map(_.length).max
    words.forall(stringSet.contains) shouldBe true
  }

  test("insert 10000 English words in StringSet") {
    import scala.util.Random
    val words = Source.fromResource("words.txt").getLines.to(Seq)
    val stringSet = StringSet()
    Random.shuffle(words ++ words).foreach(stringSet.insert)
    stringSet.size shouldBe words.size
    stringSet.depth shouldBe words.map(_.length).max
    words.forall(stringSet.contains) shouldBe true
  }

  test("insert 10000 English words in index") {
    import scala.util.Random
    val words = Source.fromResource("words.txt").getLines.to(Seq)
    val stringIndex = StringIndex()
    val testWords = Random.shuffle(words ++ words)
    testWords.zipWithIndex.foreach(Function.tupled(stringIndex.insert))
    words.forall { word =>
      val refs = Set(testWords.indexOf(word), testWords.lastIndexOf(word))
      stringIndex.find(word) == refs
    } shouldBe true
  }

  test("Delete works properly for sets") {
    import scala.util.Random
    val words = Source.fromResource("words.txt").getLines.to(Seq)
    val stringSet = StringSet()
    val testWords = Random.shuffle(words ++ words)
    testWords.foreach(stringSet.insert)
    val shuffled = Random.shuffle(words)
    val deleted = shuffled.slice(0, words.length/2)
    val tested = shuffled.slice(words.length/2, words.length)
    deleted.foreach(stringSet.delete)
    stringSet.size shouldBe words.length / 2
    tested.forall(stringSet.contains) shouldBe true
    deleted.exists(stringSet.contains) shouldBe false
  }

  test("Delete works properly for indices") {
    import scala.util.Random
    val words = Source.fromResource("words.txt").getLines.to(Seq)
    val stringIndex = StringIndex()
    val testWords = Random.shuffle(words ++ words).zipWithIndex
    testWords.foreach(Function.tupled(stringIndex.insert))
    val shuffled = Random.shuffle(testWords)
    val deleted = shuffled.slice(0, testWords.length/2)
    val tested = shuffled.slice(testWords.length/2, testWords.length)
    deleted.foreach(Function.tupled(stringIndex.delete))
    stringIndex.size shouldBe testWords.length / 2
    tested.forall{ case (e, i) => stringIndex.find(e).contains(i) } shouldBe true
    deleted.forall{ case (e, i) => ! stringIndex.find(e).contains(i) } shouldBe true
  }

  ignore("find the longest word") {
    val words = Source.fromResource("words.txt").getLines.to(Seq)
    println(words.map(w => (w.length, w)).sorted.reverse.take(20))
  }
