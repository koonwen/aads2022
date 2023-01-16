@main def week03EntryPoint(): Unit =
  println("Welcome to Week03")

/**
 * Memoization
 * ===========
 * For pure functions, we can cache the results of the function for all encountered arguments,
 * so as to be reused whenever the same arguments are encountered again. This technique will lead
 * to a solution pattern called *Dynamic Programming*.
 */

def fib: BigInt => BigInt = { // exponential time complexity
  case n if n < 2 => n
  case n => fib(n - 1) + fib(n - 2)
}

object fibMemo:
  private val memo = collection.mutable.HashMap[BigInt, BigInt]()
  def apply(n: BigInt): BigInt = compute(n)
  def compute: BigInt => BigInt = { // linear time complexity because of memoing
    case n if n < 2 => n
    case n => memo.getOrElseUpdate(n, compute(n-1) + compute(n-2))
  }

/**
 * Dynamic Programming
 * ===================
 */

def editDistanceNaive(target: String, editable: String): Int =
  def helper: (List[Char], List[Char]) => Int = {
    case (h1 :: t1, h2 :: t2) if h1 == h2 => helper(t1, t2)
    case (tgt@h1 :: t1, edt@h2 :: t2) => List(helper(tgt, t2), helper(t1, edt)).min + 1
    case (Nil, edt) => edt.length
    case (tgt, Nil) => tgt.length
  }
  helper(target.to(List), editable.to(List))

def editDistanceMemo(target: String, editable: String): Int =
  lazy val memo = collection.mutable.HashMap[(List[Char], List[Char]), Int]()
  def helper: (List[Char], List[Char]) => Int = {
    case (h1 :: t1, h2 :: t2) if h1 == h2 => memo.getOrElseUpdate((t1, t2), helper(t1, t2))
    case (tgt @ h1 :: t1, edt @ h2 :: t2) => 1 + Math.min(
      memo.getOrElseUpdate((tgt, t2), helper(tgt, t2)),
      memo.getOrElseUpdate((t1, edt), helper(t1, edt))
    )
    case p @ (Nil, edt) => memo.getOrElseUpdate(p, edt.length)
    case p @ (tgt, Nil) => memo.getOrElseUpdate(p, tgt.length)
  }
  helper(target.to(List), editable.to(List))

def editDistanceDP(target: String, editable: String): Int =
  val args = List((target.length, target), (editable.length, editable))
  val (shorter, longer) = (args.min._2, args.max._2)
  val memo = Array.fill(3, shorter.length+1)(0)
  memo(1)(0) = 1 ; if shorter.nonEmpty then memo(1)(1) = 1
  def sameChar(i: Int, diag: Int) = i > 0 && i < diag && longer(diag-i-1) == shorter(i-1)
  def acc(cond: Boolean, value: => Int) = Option(Int.MaxValue).filter(_ => cond).getOrElse(value)
  def minCell(i: Int, diag: Int) = Math.min(acc(i <= 0, memo((diag-1) % 3)(i-1)), acc(i >= diag, memo((diag-1) % 3)(i)))
  for
    diag <- 2 to longer.length + shorter.length
    i <- Math.max(0, diag - longer.length) to Math.min(diag, shorter.length)
  yield
    memo(diag % 3)(i) = if sameChar(i, diag) then memo((diag-2) % 3)(i-1) else 1 + minCell(i, diag)
  memo((shorter.length + longer.length) % 3)(shorter.length)

/**
 * The 0/1 Knapsack problem.
 * =========================
 * We have a container of capacity C, and a set of items, each with a size (or weight, if you prefer) and a value.
 * We can only put whole items in the container (that's why it's called 0/1). It is likely that not all items will
 * fit together inside the container, so we have to choose.
 * The overall size of the chosen items cannot exceed the capacity.
 * Return the maximum value that can be obtained by adding items to the container.
 */
case class Item(size: Int, value: Int)

def knapsackNaive(capacity: Int, items: Array[Item]): Int =
  def helper: (Int, Int) => Int = {
      case (cap, _) if cap <= 0 => 0
      case (cap, 0) if items(0).size <= cap => items(0).value
      case (_, 0) => 0
      case (cap, upTo) =>
        val remainingCap = cap - items(upTo).size
        val valueIfUsing = Option(remainingCap).filter(_ >= 0).map(helper(_, upTo - 1) + items(upTo).value)
        val valueIfNotUsing = Option(helper(cap, upTo - 1))
        List(valueIfUsing, valueIfNotUsing).flatten.max
  }
  helper(capacity, items.length-1)

def knapsackDP(capacity: Int, items: Array[Item]): Int =
  val dp = Array.fill(2, capacity + 1)(0)
  (0 to capacity).foreach(i => dp(0)(i) = if i < items(0).size then 0 else items(0).value)
  for
    i <- 1 until items.length
    j <- 0 to capacity
  yield
    val remainingCap = j - items(i).size
    val valueIfUsing = Option(remainingCap).filter(_ >= 0).map(dp((i-1) % 2)(_) + items(i).value)
    val valueIfNotUsing = Option(dp((i-1) % 2)(j))
    dp(i % 2)(j) = List(valueIfUsing, valueIfNotUsing).flatten.max
  dp((items.length-1) % 2)(capacity)

class Week03 // keep IntelliJ happy
