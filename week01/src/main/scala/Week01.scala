@main
def week01EntryPoint(): Unit =
  println("Welcome to Week01")

  val anInt: Int = 42
  println(s"The meaning of life, the universe, and everything is $anInt")

  var aMutableInt = 2 // Discouraged, use only when really needed, e.g. for efficiency
  println(s"Current value of aMutableInt is: $aMutableInt")
  aMutableInt *= 2
  println(s"After mutation the value of aMutableInt is $aMutableInt")

  val s: String = if aMutableInt + 40 > anInt then "greater" else "smaller or equal"
  println(s"The result of the comparison is \"$s\"")

  def aMethod(a: Int, b: Int): Int = a * a + b * b
  println(s"The result of aMethod($anInt, $aMutableInt) is ${aMethod(anInt, aMutableInt)}")

  def aSideEffectMethod(a: Int): Unit =
    println(s"The argument is: $a")
    println(s"The square of the argument is: ${a*a}")

  aSideEffectMethod(anInt * aMutableInt)

// Globally defined method
def anOutOfScopeMethod(s1: String, s2: String): String = s1.toUpperCase + s2.toLowerCase

def fibRecursive(n: Int): Int = if n < 2 then n else fibRecursive(n-1) + fibRecursive(n-2)

def fibTailRecursive(n: Int): Int =
  @scala.annotation.tailrec
  def helper(n: Int, a: Int, b: Int): Int = if n == 0 then a else helper(n-1, b, a+b)
  helper(n, 0, 1)

@main
def week01SecondEntryPoint(): Unit =
  println(s"anOutOfScopeMethod result is ${anOutOfScopeMethod("abcd", "DEFG")}")
  println(s"The value of fibRecursive(10) is ${fibRecursive(10)}")
  println(s"The value of fibTailRecursive(10) is ${fibTailRecursive(10)}")

// Move on to Testing in Week01Test.scala

// Curly braces
@main
def week01ThirdEntryPoint(): Unit =
  val complicatedExpression1 = {
    val a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9
    val b = 10 + 11 + 12 + 13 + 14 + 15
    a * b
  }
  val complicatedExpression2 = {
    val a = BigInt("1234" * 5)
    val b = BigInt("34" * 3 + "56" * 4)
    a + b
  }
  val finalResult = complicatedExpression1 + complicatedExpression2
  println(s"The final result is: $finalResult")

  def curriedFunction(a: String)(b: String): BigInt =
    val aReplicated = a * 10
    val bReplicated = b * 11
    BigInt(aReplicated + bReplicated)

  println(s"curriedFunction(\"98\")(\"87\") yields ${curriedFunction("98")("87")}")

  val finalResult2 = curriedFunction("23") { // second argument is multiline
    val c = "45" * 4
    val d = 78 * 3
    c + d
  }
  println(s"finaResult2 is $finalResult2")

// Lambdas -- methods and lambdas are not exactly the same thing, but are convertible to each other

def aMethod(a: Int): Int = a + 1

val aLambda: Int => Int = i => i + 1

val aLambdaFromMethod: Int => Int = aMethod

val aLambdaWithShortcut: Int => Int = _ + 1 // underscore represents the argument when the argument occurs only once

val aBinaryLambda: (Int, Int) => Int = (x, y) => x + y // (x, y) not pair, but 2 args

val aDesugaredLambda: Int => Int = new Function[Int, Int] {
  def apply(a: Int): Int = a + 1
}

val arrowTypeIsJustSyntacticSugar: Function[Int, Int] = aLambda // syntactically correct

// Objects
// A class is a record, and thus a product type

case class ClassExample(a: Int, b: String, c: Boolean): // Aggregate values of different types together
  def methodExample: String =
    val optionalPlural = if a == 1 then "" else "s"
    if c then s"$a $b$optionalPlural" else "Not telling you"

  val d: String = b * a

  val lambdaExample: Int => String = b * _ // shortcut for n => b * n

  def e: Int = a * a // recomputed every time d is accessed

  def f: String => String = s => (b + s).reverse  // s.reverse + b.reverse

@main
def week01FourthEntryPoint(): Unit =
  val o1 = ClassExample(1, "dog", true)
  println(s"o1 = $o1")
  val o2 = ClassExample(2, "cat", true)
  println(s"o2 = $o2")
  val o3 = ClassExample(3, "bird", false)
  println(s"o3 = $o3")

  println(s"o1.methodExample returns ${o1.methodExample}")
  println(s"o2.methodExample returns ${o2.methodExample}")
  println(s"o3.methodExample returns ${o3.methodExample}")
  println(s"o2.d = ${o2.d}")
  println(s"o2.lambdaExample(3) = ${o2.lambdaExample(3)}")
  println(s"o2.e = ${o2.e}") // recomputed every time it is invoked
  println(s"o2.f(\"esuom\") = ${o2.f("esuom")}")

// Sum types. Type recursion
//==========================

sealed trait TreeNode:
  def content: String
case class Leaf(content: String) extends TreeNode
case class UnaryNode(content: String, child: TreeNode) extends TreeNode
case class BinaryNode(content: String, left: TreeNode, right: TreeNode) extends TreeNode

def preorderTreeTraversal(t: TreeNode): String =
  t match
    case Leaf(s) => s
    case UnaryNode(s, ct) => s + preorderTreeTraversal(ct)
    case BinaryNode(s, left, right) => s + preorderTreeTraversal(left) + preorderTreeTraversal(right)

@main
def week01FifthEntryPoint(): Unit =
  val t: TreeNode = BinaryNode("a", UnaryNode("b", Leaf("c")), BinaryNode("d", Leaf("e"), Leaf("f")))
  println(s"The tree traversal of $t is ${preorderTreeTraversal(t)}")

// Leaf is a subtype of Tree; UnaryNode is a subtype of Tree; BinaryNode is a subtype of tree.

// More on traits and classes. Subtyping
//=====================================

import scala.collection.WithFilter
import util.Random.{nextInt, nextString}
trait TraitA:
  val a: Int = nextInt

trait TraitB:
  val b: String = nextString(5)

trait TraitC extends TraitA with TraitB:
  val c: BigInt = BigInt(nextInt)

case class ClassExtendingTraits(d: Int) extends TraitC:
  override def toString: String = s"ClassExtendingTraits($d, $c, $b, $a)"

@main
def week01SixthEntryPoint(): Unit =
  println(s"Object of type ClassExtendingTraits: ${ClassExtendingTraits(nextInt)}")

val subtype1: TraitA = ClassExtendingTraits(1) // compiles

val subtype2: TraitB = ClassExtendingTraits(2) // compiles

val subtype3: TraitC = ClassExtendingTraits(3) // compiles

val subtype4: TraitA with TraitB = subtype3

val subtype5: TraitA & TraitB = subtype3

type TraitAB = TraitA & TraitB

// Sum types -- Alternative for trees
//===================================

enum GTree[+T]: // Scala 3 only
  case GLeaf
  case GNode(content: T, left: GTree[T], right: GTree[T])
import GTree.{GLeaf, GNode}

def treeHeight[T](t: GTree[T]): Int = // tested in Week01Test.scala
  t match {
    case GLeaf => 0
    case GNode(_, left, right) => 1 + Math.max(treeHeight(left), treeHeight(right))
  }

// Alternative, more concise syntax for lambdas
def treeHeight2[T]: GTree[T] => Int =
  case GNode(_, left, right) => 1 + Math.max(treeHeight2(left), treeHeight2(right))
  case GLeaf => 0

// Covariance, contravariance

class Person(val name: String):
  override def toString: String = s"Person($name)"

class Student(studentName: String, val major: String) extends Person(studentName):
  override def toString: String = s"Student($name, $major)"

// A Student IS a Person. A Person is NOT necessarily a Student (maybe yes, maybe no)

val personToStudent: Person => Student = person => Student(person.name + " the young", "English")
val studentToPerson: Student => Person = student => Person(student.name + " BSc")

val peopleChanger: Array[Student => Person] = Array(personToStudent, studentToPerson)

// Student => Person >: Person => Student
//
// Arg type: contravariant
// Result type: covariant
//
// In fact    Student => Person >: Person => Person   >: Person => Student
//            Student => Person >: Student => Student >: Person => Student
//

@main
def week01SeventhEntryPoint(): Unit =
  val p = Person("John")
  val s = Student("George", "CS")
  //println(s"$p $s")
  val people: Array[Person] = peopleChanger.map(_(s))
  people.foreach(println)

// Arrow notation is just syntactic sugar to form an object of type 'Function'
val personToStudentNoSynSugar: Function[Person, Student] = new Function[Person, Student] {
  def apply(p: Person): Student = Student(p.name + " the young", "English")
}

// Most parametric types are co-variant. 'Function' is one notable exception, in having its first type argument
// as contra-variant

// Cartesian Product
//==================

@main
def week01EightEntryPoint(): Unit =
  val pair = (3, "three")
  println(s"pair = $pair, components: ${pair._1}; ${pair._2}")
  val triple = (4, 4.0, "four")
  val quintuple:(Int, String, Int, Double, String) = pair ++ triple
  println(s"quintuple = $quintuple}, arity = ${quintuple.productArity}")
  val single = Tuple1(3) // bracket notation is just syntactic sugar for a Tuple case class
  println(s"(single._1 == 3) = ${single._1 == 3}")
  val (one, two, three, four, five) = quintuple // pattern matching works in initialization and assignment
  println(s"one = $one; two = $two; three = $three; four = $four; five = $five")
  // pattern matching does not work in function arguments
  def rotateTriple[T1, T2, T3](triple: (T1, T2, T3)): (T2, T3, T1) = (triple._2, triple._3, triple._1)
  println(s"rotated triple = ${rotateTriple(triple)}")
  // lambdas can pattern match
  def rotatedQuintuple[T1, T2, T3, T4, T5]: ((T1, T2, T3, T4, T5)) => (T2, T3, T4, T5, T1) =
    case (a, b, c, d, e) => (b, c, d, e, a)
  println(s"rotated quintuple: ${rotatedQuintuple(quintuple)}")
  println(s"rotation of (1,2,3,4,5) is ${rotatedQuintuple((1,2,3,4,5))}")
  println(s"rotation of Tuple5(1,2,3,4,5) is ${rotatedQuintuple(Tuple5(1,2,3,4,5))}")

// IMPORTANT: a function taking n arguments is different than a function taking 1 arg that is an n-tuple
// To have an n-tuple argument, we need 2 level of brackets

// More tuple properties in the Week01Test.scala

// Immutable Collections
//======================

val aList: List[Int] = List(1, 2, 3, 4, 5)  // Usually elements of same type
val aHeterogeneousList: List[Any] = List(1, "2", 3, "4", 5)
// More list properties in Week01Test.scala

@main def week01ListEntryPoint(): Unit =
  def reverseList[T](l: List[T]): List[T] = l.foldLeft(List.empty[T]) { (r, e) => e :: r }
  println(s"The reverse of List(a, b, c) is ${reverseList(List("a", "b", "c"))}")


val anOption: Option[Int] = Some(10)
val anEmptyOption: Option[Int] = None
val anOptionHandlingNull: Option[String] = Option(null) // evaluates to None

val anArray: IArray[String] = IArray("a", "b", "c", "d")
val anArrayElem: String = anArray(3) // returns "d"; Arrays are similar to functions, the index is just like an argument

// More array properties in Week01Test.scala

import collection.immutable.{TreeSet, HashSet}

val aSet: Set[String] = Set("a", "b", "c")
val aBoolean: Boolean = aSet("a") // returns true; Sets are like predicates, take element as argument and return true/false
val anOrderedSet: Set[Int] = TreeSet(2,3,4) // red-black tree, O(log n) membership test, sorting possible, bounds calculation possible
val aHashSet: Set[Int] = HashSet(5,3,1) // Standard hash set
// More Set properties in Week01Test.scala

import collection.immutable.{TreeMap, HashMap}

val aMap: Map[String, Int] = Map( "a" -> 1, "b" -> 2, "c" -> 3) // key -> value pairs. a -> b is same as (a,b)
val aNewInt: Int = aMap("a") // returns 1; a Map is like a function, take key as argument and return value
val anOrderedMap: Map[String, Int] = TreeMap( "a" -> 1, "b" -> 2, "c" -> 3)  // Red-black tree, O(log n) access time
val aHashdMap: Map[String, Int] = HashMap( "a" -> 1, "b" -> 2, "c" -> 3)  // Standard hash map, O(1) access
// More Map properties in Week01Test.scala

@main def week01StreamEntryPoint(): Unit =
  lazy val aStream: LazyList[Int] = 1 #:: aStream.map(_ + 1) // infinite stream of positive numbers
  println(aStream.take(10).force)
  lazy val fibs: LazyList[Int] = 0 #:: 1 #:: (fibs zip fibs.tail).map { case (a, b) => a + b }
  println(fibs.take(20).force)
  val fibs2 = LazyList.iterate((0, 1)){ case (a, b) => (b, a+b) }.map(_._1)
  println(fibs2.take(20).force)
  def primesFrom(n: Int): LazyList[Int] = n #:: primesFrom(n+1).filter(_ % n != 0)
  val primes = primesFrom(2)
  println(primes.take(30).force)

@main def week01IteratorEntryPoint(): Unit =
  lazy val ints: Iterator[Int] = Iterator.iterate(1) { _ + 1 }
  println(ints.take(20).to(List))
  def primesFrom(n: Int): Iterator[Int] = Iterator(n) ++ primesFrom(n+1).filter(_ % n != 0)
  val primes = primesFrom(2)
  println(primes.take(30).to(List))

  def primesIterative(n: Int): Set[Int] = Iterator.iterate((TreeSet(2), 3)) {
    case (p, i) if p.takeWhile(k => k*k <= i).forall(i % _ != 0) => (p + i, i + 2)
    case (p, i) => (p, i+2)
  }.dropWhile(_._1.size < n).next._1

  println(s"First 30 primes: ${primesIterative(30)}")

  // Utility iterators

  (1 to 10).foreach(i => println(s"Iterator's current value: $i")) // 'to' - inclusive; 'until' - right exclusive
  println(s"Range string: ${('g' to 'm').mkString("[",",","]")}")

// Mutable Collections
//====================

@main
def week01ArraysEntryPoint(): Unit =
  val mutableArray: Array[Int] = Array(10, 20, 30, 40, 50)
  mutableArray(2) = mutableArray(4)
  println(s"Array after mutation: [${mutableArray.mkString(",")}]")

@main
def week01MutableSetEntryPoint(): Unit =
  val mutableSet = collection.mutable.Set("a", "b", "c", "a")
  println(s"Set before mutation: $mutableSet")
  mutableSet += "d"
  println(s"Set after mutation: $mutableSet")
  mutableSet += "b"
  println(s"Set after trying to add a value already in the set: $mutableSet")
  mutableSet -= "b"
  println(s"Set after removing a value: $mutableSet")
  mutableSet -= "z"
  println(s"Set after trying to remove a non-existent value: $mutableSet")

@main
def week01MutableMapEntryPoint(): Unit =
  val mutableMap = collection.mutable.Map("a" -> 1, "b" -> 2, "c" -> 3)
  println(s"Map before mutation: $mutableMap")
  mutableMap("b") = 10
  mutableMap("d") = 20
  println(s"Map after mutation: $mutableMap")
  mutableMap -= "d"
  mutableMap -= "e"
  println(s"Map after removing keys: $mutableMap")

@main
def week01MutablePriQEntryPoint(): Unit =
  val priQ = collection.mutable.PriorityQueue(20, 10, 40, 50, 20, 60, 100, 80, 90, 50, 30)
  println(s"PriQ before mutation: $priQ")
  println(s"PriQ root value: ${priQ.head}")
  println(s"Dequeued value: ${priQ.dequeue()}")
  println(s"PriQ after dequeueing: $priQ")
  priQ.enqueue(45, 55, 85, 15, 26, 75, 65, 95)
  println(s"PriQ after enqueueing: $priQ")
  println(s"PriQ current maximum: ${priQ.head}")

@main
def week01MutableQueueEntryPoint(): Unit =
  val queue = collection.mutable.Queue(10, 20, 30)
  println(s"The initial queue: $queue")
  queue += 40
  println(s"The queue after an insert: $queue")
  val qElem = queue.dequeue
  println(s"The queue after a dequeue: $queue; extracted element: $qElem")

// for comprehension

@main def week01ForComprehensionEntryPoint(): Unit =
  // Cartesian product
  val cartesianProduct: Set[(Int, String, Boolean)] = for
    e1 <- Set(10, 20, 30, 40, 50, 60)
    e2 <- Set("alice", "bob", "charles")
    e3 <- Set(true, false)
  yield (e1, e2, e3)
  println(s"The cartesian product is: $cartesianProduct")

  // All combinations (i,j) where i < j
  val intCombinations: IndexedSeq[(Int, Int)] = for
    j <- 1 to 10
    i <- 1 to j
  yield (i, j)
  println("Int combinations (i,j) where i < j")
  intCombinations.foreach(println)

  // All triples whose squares add up to a perfect square
  val perfectSquares: IndexedSeq[(Int, Int, Int)] = for
    j <- 1 to 50
    i <- 1 to j
    k = Math.sqrt(i*i + j*j).round.toInt
    if i*i + j*j == k*k
  yield (i, j, k)
  println("Perfect squares")
  perfectSquares.foreach(println)

@main def week01ComprehensionsDesugaredEntryPoint(): Unit =
  // Cartesian product desugared
  val cartesianProduct: Set[(Int, String, Boolean)] =
    Set(10, 20, 30, 40, 50, 60).flatMap(e1 =>
      Set("alice", "bob", "charles").flatMap(e2 =>
        Set(true, false).map(e3 =>
          (e1, e2, e3)
        )
      )
    )
  println(s"The desugared cartesian product is: $cartesianProduct")

  // All combinations (i,j) where i < j, desugared
  val intCombinationsDesugared: IndexedSeq[(Int, Int)] =
    (1 to 10).flatMap(j =>
      (1 to j).map(i =>
        (i, j)
      )
    )
  println("Int combinations (i,j) where i < j, desugared")
  intCombinationsDesugared.foreach(println)

  // All triples whose squares add up to a perfect square
  val perfectSquaresDesugared: IndexedSeq[(Int, Int, Int)] =
    (1 to 50).flatMap(j =>
      (1 to j).flatMap(i =>
        IndexedSeq(Math.sqrt(i * i + j * j).round.toInt)
          .withFilter(k =>
            i * i + j * j == k * k
          ).map(k =>
            (i, j, k)
          )
      )
    )
  println("Perfect squares, desugared")
  perfectSquaresDesugared.foreach(println)
