@main
def week02EntryPoint(): Unit =
  println("Welcome to Week02")

// Reactive programming
// ====================

import io.Source

@main def weightsTree(): Unit =
  val employeeMap = readTreeFromFile("treespec.csv")
  println(employeeMap)
  val root = getRoot(employeeMap).get // We let it throw if the file is incorrect
  // println(s"Root of tree read from file: $root")
  if isTree(employeeMap, root) then () else throw new RuntimeException("Unable to read tree from file")
  // println("Employees under Charles, computed recursively (subtreeNodes1)")
  subtreeNodes1(employeeMap, "charles").sorted.foreach(println)
  // println("Employees under Charles, computed with 'For' comprehension (subtreeNodes2)")
  subtreeNodes2(employeeMap, "charles").sorted.foreach(println)
  // println("Employees under Charles, computed iteratively (subtreeNodes3)")
  subtreeNodes3(employeeMap, "charles").sorted.foreach(println)

  val weights = readWeightsFromFile("weights.csv")
  val tw = teamWeight("charles", employeeMap, weights, subtreeNodes1)
  // println(s"team weight = $tw")

// Read a tree from a resource file
def readTreeFromFile(fileName: String): Map[String, List[String]] =
  val Edge = "([^,]+),(.+)".r
  Source.fromResource(fileName).getLines.map {
    case Edge(l, r) => (l, r)
  }
    .foldLeft(Map.empty[String, List[String]]) {
      case (m, (l, r)) => m + (l -> (r :: m.getOrElse(l, List())))
    }
//    .to(List).groupMap(_._1)(_._2)

// Compute the root of a tree generated as a Map (since not all maps are valid trees).
def getRoot(t: Map[String, List[String]]): Option[String] =
  val innerNodes = t.keySet
  val children = innerNodes.flatMap(t(_)).to(Set)
  val possibleRoot = innerNodes.diff(children)
  if possibleRoot.size != 1 then None else possibleRoot.headOption

// Check if a generated map represents a tree indeed
def isTree(t: Map[String, List[String]], root: String): Boolean =
  val rootDoesNotAppearOnRightSide = t.keys.forall(l => !t(l).toSet(root))
  val adjacencyListsAreDisjoint: Iterable[Boolean] = for
    i <- t.keys
    j <- t.keys
    if i != j
  yield t(i).toSet.intersect(t(j).toSet).isEmpty
  rootDoesNotAppearOnRightSide && adjacencyListsAreDisjoint.forall(identity)

// Compute all the nodes under a sub-root recursively
def subtreeNodes1(tree: Map[String, List[String]], name: String): List[String] =
  name :: tree.getOrElse(name, List.empty[String]).flatMap(subtreeNodes1(tree, _))

// Compute all the nodes under a sub-root as for comprehensions
def subtreeNodes2(tree: Map[String, List[String]], name: String): List[String] = name :: {
  for {
    child <- tree.getOrElse(name, List.empty[String])
    childrenDescendants <- subtreeNodes2(tree, child)
  } yield childrenDescendants
}

// Compute all the nodes under a sub-root iteratively
def subtreeNodes3(tree: Map[String, List[String]], name: String): List[String] =
  Iterator.iterate((List(name), tree.getOrElse(name, List.empty[String]))) {
    case (o, e::s) => (e::o, tree.getOrElse(e, List.empty[String]) ++ s)
    case p => p // p._2 should be Nil here
  }.dropWhile(_._2.nonEmpty).next._1

// Read weights from file
def readWeightsFromFile(fileName: String): Map[String, Int] =
  val Row = "([^,]+),(.+)".r
  Source.fromResource(fileName).getLines.map {
    case Row(name, amount) => name -> amount.toInt
  }.to(Map)

// Compute the cumulated weight of all the nodes under a sub-root
def teamWeight(
  name: String,
  tree: Map[String,List[String]],
  weights: Map[String, Int],
  subTreeNodes: (Map[String, List[String]], String) => List[String]
): Int = subTreeNodes(tree, name).flatMap(weights.get).sum

// Compute the height of a generated tree (useful when dealing with automatically generated random tree).
def subtreeNodeHeight(tree: Map[String, List[String]], name:String): Int =
  1 + (0::tree.getOrElse(name, List.empty[String]).map(subtreeNodeHeight(tree, _))).max


// Estimating complexity
// =====================

// Swap two elements of an array. Very common operation when sorting.
def swap(i: Int, j: Int)(using arr: Array[Int]): Unit = { val tmp = arr(i); arr(i) = arr(j); arr(j) = tmp }

// The well-known bubble sort algorithm, whose complexity we want to estimate
def bubbleSort(arr: Array[Int]): Unit =
  given Array[Int] = arr
  (arr.length-1 to 0 by -1).foreach{ i => (0 until i).foreach{ j => if arr(j) > arr(j+1) then swap(j, j+1)}}

// The same algorithm, with for-comprehensions
def bubbleSort2(arr: Array[Int]): Unit =
  given Array[Int] = arr
  for
    i <- arr.length-1 to 0 by -1
    j <- 0 until i
    if arr(j) > arr(j+1)
  yield swap(j, j+1)


// Solve the problem "Consensus and Profile" from Rosalind.info
//=============================================================
// Problem statement:
// Consider a set (list) of N DNA strands of the same length. For each position in (0 until N-1), take the most
// popular/frequent nucleotide symbol at that position across the entire set of DNA strands given as input.
// The DNA string obtained by using the most popular nucleotide symbol at each position is the "consensus" of
// the set of DNA strands. The problem is asking to compute the consensus string for the input set of strands.

def consensus: List[String] => String =
  _.map(_.to(List)).transpose.map(_.groupBy(identity).map { case (k, v) => (v.size, k) }.max._2).mkString("")