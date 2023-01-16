import IHeap.singleton

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance

@main
def week05EntryPoint(): Unit =
  println("Welcome to Week05")

// Immutable lists: all operations return NEW lists where some of the original list structure may be shared
// All operations are constant time, except _append_ (++++) and _reverse_
// Reverse would be O(n) even for mutable lists; Append could be constant time for mutable lists.
// Thought Mutable Append would destroy the first list.
// Append copies the first list, but shares the second, and operates in O(n).
sealed trait IList[+T]:
  def length: Int
  def head: T
  def headOption: Option[T]
  def tail: IList[T]
  def isEmpty: Boolean
  def ::::[T1 >: T](elem: T1): IList[T1] = new ::::(elem, this)
  def reverse: IList[T] = Iterator.iterate((this, INil: IList[T])) {
    case (h::::t, r) => (t, h::::r)
    case (INil, r) => assert(false)
  }.dropWhile(!_._1.isEmpty).next._2
  def ++++[T1 >: T](that: IList[T1]): IList[T1] = Iterator.iterate((reverse, that)) {
    case (h::::t, r) => (t, h::::r)
    case (INil, r) => assert(false)
  }.dropWhile(!_._1.isEmpty).next._2
  def append[T1 >: T](that: IList[T1]): IList[T1] = this match {
    case h :::: t => h :::: t.append(that)
    case INil => that
  }
  

case class ::::[+T](override val head: T, override val tail: IList[T]) extends IList[T]: // h :::: t
  override def length: Int = tail.length + 1
  override def isEmpty: Boolean = false
  override def headOption: Option[T] = Option(head)

case object INil extends IList[Nothing]:
  override def length: Int = 0
  override def head: Nothing = throw new NoSuchElementException("Attempting to get the head of an empty list")
  override def headOption: None.type = None
  override def tail: Nothing = throw new NoSuchElementException("Attempting to get the tail of an empty list")
  override def isEmpty: Boolean = true

object IList:
  def apply[T](args: T*): IList[T] = Iterator.iterate((args, INil: IList[T])) {
    case (args, r) if args.nonEmpty => (args.tail, args.head :::: r)
    case _ => assert(false)
  }.dropWhile(_._1.nonEmpty).next._2.reverse

// Immutable stacks are just lists, performance of main operations remains constant time

case class IStack[+T](list: IList[T]):
  def push[T1 >: T](elem: T1): IStack[T1] = new IStack(elem :::: list)
  def peek: T = if !list.isEmpty then list.head else throw new NoSuchElementException("Peek at empty stack")
  def peekOption: Option[T] = list.headOption
  def pop: (T, IStack[T]) = list match {
    case h :::: t => (h, new IStack(t))
    case INil => throw new NoSuchElementException("Pop from empty stack")
  }
  def popOption: Option[(T, IStack[T])] = list.headOption.map(_ => pop)
  def isEmpty: Boolean = list.isEmpty

object IStack:
  def apply[T](args: T*) = new IStack(IList(args: _*))

// Immutable queue rely on IList.reverse to accomplish _amortized_ constant time
case class IQueue[+T](list: IList[T], rlist: IList[T]):
  def enqueue[T1 >: T](elem: T1): IQueue[T1] = new IQueue(elem::::list, rlist)
  def isEmpty: Boolean = list.isEmpty && rlist.isEmpty
  def dequeueOption: Option[(T, IQueue[T])] = // Amortized complexity: O(n)
    rlist
      .headOption
      .map((_, new IQueue(list, rlist.tail)))
      .orElse {
        val r = list.reverse
        r.headOption.map((_, new IQueue(INil, r.tail)))
      }
  def dequeue: (T, IQueue[T]) = dequeueOption.getOrElse(throw new NoSuchElementException("Dequeue from empty queue"))
  def peekOption: Option[T] = rlist.headOption.orElse(list.reverse.headOption)
  def peekOptionAndCleanUp: Option[(T, IQueue[T])] = rlist.headOption.map((_, this)).orElse {
    dequeueOption match {
      case Some((r, q)) => Option((r, IQueue(INil, r :::: q.rlist)))
      case None => None
    }
  }
  def peek: T = peekOption.getOrElse(throw new NoSuchElementException("Peek at empty queue"))

object IQueue:
  def apply[T](args: T*): IQueue[T] = new IQueue[T](INil, IList(args: _*).reverse)

// Immutable but delayed variable that can be assigned only once, but later than its creation time.
// This is still safe from a concurrency standpoint.

class DelayedVal[T]:
  private[this] var _value: Option[T] = None
  def value: Option[T] = synchronized { _value }
  def update(v: T): Unit = synchronized {
    if value.isEmpty then _value = Option(v) else throw new IllegalStateException("delayed val already assigned")
  }
  def tryUpdate(v: T): (T, Boolean) = synchronized {
    if value.isEmpty
    then
      _value = Some(v)
      (v, true)
    else
      (value.get, false)
  }

object DelayedVal:
  def apply[T]() = new DelayedVal[T]

// Immutable trees -- leftist heaps

sealed trait ITree[T]:
  def size: Int
  def depth: Int
  def rank: Int // "right spine" length to a leaf (i.e. number of "right" links from current node to a leaf)

case class ILeaf[T]() extends ITree[T]:
  val size: Int = 0
  val depth: Int = 0
  val rank: Int = 0

case class INode[T](value: T, left: ITree[T], right: ITree[T], size: Int, depth: Int, rank: Int) extends ITree[T]

import Ordered.orderingToOrdered

case class IHeap[T: Ordering](tree: ITree[T]):
  def isEmpty: Boolean = tree.isInstanceOf[ILeaf[T]]

  def withoutMin: IHeap[T] = tree match {
    case ILeaf() => throw new NoSuchElementException("Deletion invoked on an empty heap")
    case INode(_, left, right, _, _, _) => IHeap(left).merge(IHeap(right))
  }

  def getMin: Option[(T, IHeap[T])] = tree match {
    case ILeaf() => None
    case INode(x, _, _, _, _, _) => Option((x, withoutMin))
  }

  def peekMin: Option[T] = tree match {
    case ILeaf() => None
    case INode(x, _, _, _, _, _) => Option(x)
  }

  def insert(value: T): IHeap[T] = merge(singleton(value))

  def merge(other: IHeap[T]): IHeap[T] =
    def makeTree(rootVal: T, subtree1: ITree[T], subtree2: ITree[T]) =
      val subtrees = List(subtree1, subtree2)
      val newSize = subtrees.map(_.size).sum + 1
      val newDepth = subtrees.map(_.depth).max + 1
      if subtree1.rank >= subtree2.rank
      then INode(rootVal, subtree1, subtree2, newSize, newDepth, subtree2.rank + 1)
      else INode(rootVal, subtree2, subtree1, newSize, newDepth, subtree1.rank + 1)

    def helper: (ITree[T], ITree[T]) => ITree[T] = {
      case (ILeaf(), t) => t
      case (t, ILeaf()) => t
      case (INode(x, t1l, t1r, _, _, _), t2 @ INode(y, _, _, _, _, _)) if x <= y => makeTree(x, helper(t1r, t2), t1l)
      case (t1, INode(y, t2l, t2r, _, _, _)) => makeTree(y, helper(t2r, t1), t2l)
    }

    IHeap(helper(tree, other.tree))

object IHeap:
  def empty[T: Ordering]: IHeap[T] = IHeap(ILeaf())

  def singleton[T: Ordering](value: T): IHeap[T] =
    IHeap[T](INode(value, ILeaf(), ILeaf(), 1, 1, 1))

// Convenient operator
import reflect.Selectable.reflectiveSelectable

implicit class ApplyDecorator[T1, T2](f : T1 => T2):
  @targetName("apply")
  def @: (a: T1): T2 = f(a)     // we can now write x @: f instead of f(x)

// The illusion of immutability
// ============================
// Immutability can be very expensive in some cases. To address that, we "hide" the mutable parts behind immutable
// interfaces or APIs.

def immutableSort[T: Ordering](l: IList[T]): IList[T] =
  type L = IList[T]
  type PF = ((L, L)) => L

  def partition(l: L, pivot: T): (L, L) =
    Iterator.iterate((l,IList[T](), IList[T]())) {
      case (h::::t, lower, higher) if h < pivot => (t, h::::lower, higher)
      case (h::::t, lower, higher) => (t, lower, h::::higher)
      case (INil, _, _) => assert(false)
    }.dropWhile(! _._1.isEmpty).next @: ((_:(L,L,L)).tail)

  sealed trait Problem
  case class Sort(l: L) extends Problem
  case class Combine(elem: T) extends Problem

  def helperR: L => L = {
    case INil => INil
    case h::::INil => h::::INil
    case h::::t => partition(t, h) @: ({ case  (lower, higher) => helperR(lower) ++++ (h :::: helperR(higher)) }: PF)
  }

  def helper(list: L): L = Iterator.iterate((IStack[Problem](Sort(list)), IStack[L]())) {
    case (problems @ IStack(Sort(l)::::_), partials) if l.length < 2 => (problems.pop._2, partials.push(l))
    case (problems @ IStack(Sort(h::::t)::::_), partials) =>
      val (l1, l2) = partition(t, h)
      (problems.pop._2.push(Combine(h)).push(Sort(l2)).push(Sort(l1)), partials)
    case (problems @ IStack(Combine(h)::::_), partials) =>
      val (l1, stack1) = partials.pop
      val (l2, stack2) = stack1.pop
      (problems.pop._2, stack2.push(l2 ++++ (h :::: l1)))
    case (problems, partials) => assert(false)
  }.dropWhile(! _._1.isEmpty).next._2.pop._1

  helper(l)

def mutableSort[T: Ordering](a: Array[T]): Unit =
  def swap(i: Int, j: Int): Unit =
    val aux = a(i)
    a(i) = a(j)
    a(j) = aux

  def partition(i: Int, j: Int): Int =
    val pivotIndex = Iterator.iterate((i+1, i+1)) {
      case (higher, unknown) if a(unknown) < a(i) => swap(higher, unknown) ; (higher+1, unknown+1)
      case (higher, unknown) if a(unknown) >= a(i) => (higher, unknown+1)
    }.dropWhile(_._2 < j).next._1 - 1
    swap(i, pivotIndex)
    pivotIndex

  def helper(i: Int, j: Int): Unit =
    if i < j then
      val pivot = partition(i, j)
      helper(i, pivot)
      helper(pivot+1, j)

  def partitionFast(i: Int, j: Int): Int =
    var higher = i+1
    var unknown = higher
    while unknown < j do
      if a(unknown) < a(i)
      then
        val aux = a(higher)
        a(higher) = a(unknown)
        a(unknown) = aux
        higher += 1
      unknown += 1
    val pivotIndex = higher - 1
    swap(i, pivotIndex)
    pivotIndex

  def helperFast(i: Int, j: Int): Unit =
    var stack = List((i, j))
    while stack.nonEmpty do
      val (i, j) = stack.head
      stack = stack.tail
      if i < j then
        val pivotIndex = partitionFast(i, j)
        stack = (i, pivotIndex) :: (pivotIndex+1, j) :: stack

  helperFast(0, a.length)

import scala.reflect.ClassTag

extension [T: Ordering : ClassTag](list: IList[T])
  def sort: IList[T] =
    var l = list
    val a: Array[T] = Array.fill(list.length)({ val current = l; l = l.tail; current.head })
    mutableSort(a)
    a.foldRight(IList[T]())( _ :::: _ )


object Week05 // keep IntelliJ happy
