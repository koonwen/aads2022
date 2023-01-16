/**
 * **Treaps:** Given records with two keys, arrange them in a tree, so that the tree is a binary search tree (BST)
 * by the first key, and a heap by the second key. This is always possible, but it's likely to result in
 * highly unbalanced trees. As a reminder, trees are efficient data containers when they are balanced, and
 * the main processing operations traverse a single path from root to a leaf. Since the balanced tree has
 * logarithmic height, the corresponding operations will have logarithmic complexity.
 *
 * However, there is still value in this idea. Suppose we have records with a single key. We generate a
 * second key randomly, and insert the node into a treap. Doing this repeatedly will result in a balanced treap
 * with high likelihood. The insertion is initially done as a BST insertion by the first key. Subsequently, the
 * heap property by the second key is restored. This insert operation is simpler (and more efficient) compared
 * with the equivalent for AVL or Red-Black trees, and thus treaps are a more practical alternative implementation
 * for balanced binary search trees.
 */

import Ordered.orderingToOrdered
import collection.mutable.TreeSet
import scala.collection.mutable
import scala.util.Random

trait Tree[T: Ordering]:
  def insert(value: T): Tree[T]
  def delete(value: T): Tree[T]
  def contains(value: T): Boolean
  def isEmpty: Boolean
  def isHeapLeft: Boolean
  def isHeapRight: Boolean
  def isHeap: Boolean = isHeapLeft && isHeapRight
  def isBST: Boolean
  def height: Int
  def size: Int

case class Empty[T: Ordering]() extends Tree[T]:
  override def insert(value: T): Tree[T] = Node(value = value, heapKey = Random.nextInt, left = Empty(), right = Empty())
  override def delete(value: T): Tree[T] = Empty()
  override def contains(value: T): Boolean = false
  override def isEmpty: Boolean = true
  override def isHeapLeft: Boolean = true
  override def isHeapRight: Boolean = true
  override def isBST: Boolean = true
  override def height: Int = 0
  override def size: Int = 0

case class Node[T: Ordering](
  value: T,
  private var heapKey: Int,
  private var left: Tree[T],
  private var right: Tree[T]
) extends Tree[T]:
  override def isEmpty: Boolean = false
  override def size: Int = left.size + right.size + 1
  override def height: Int = Math.max(left.height, right.height) + 1

  override def isBST: Boolean =
    val isBSTLeft = left match
      case l: Node[T] => l.value < this.value
      case l: Empty[T] => true
    val isBSTRight = right match
      case r: Node[T] => r.value > this.value
      case r: Empty[T] => true
    isBSTLeft && isBSTRight && left.isBST && right.isBST

  override def insert(value: T): Tree[T] =
    () match
      case _ if value == this.value => ()
      case _ if value < this.value =>
        left = left.insert(value)
        restoreHeapLeft()
      case _ =>
        right = right.insert(value)
        restoreHeapRight()
    this

  override def delete(value: T): Tree[T] = () match
    case _ if this.value == value && left.isEmpty => right
    case _ if this.value == value && right.isEmpty => left
    case _ if this.value == value =>
      heapKey = Int.MaxValue
      (left, right) match
        case (l: Node[T], r: Node[T]) if l.heapKey <= r.heapKey =>
          val newRoot = rotateRight
          newRoot.right = delete(value)
          newRoot
        case (l: Node[T], r: Node[T]) =>
          val newRoot = rotateLeft
          newRoot.left = delete(value)
          newRoot
    case _ if this.value > value =>
      left = left.delete(value)
      this
    case _ =>
      right = right.delete(value)
      this

  override def contains(value: T): Boolean = () match
    case _ if this.value == value => true
    case _ if this.value > value => left.contains(value)
    case _ => right.contains(value)

  def rotateLeft: Node[T] =
    val rightNode = right.asInstanceOf[Node[T]]
    val rightLeft = rightNode.left
    right = rightLeft
    rightNode.left = this
    rightNode

  def rotateRight: Node[T] =
    val leftNode = left.asInstanceOf[Node[T]]
    val leftRight = leftNode.right
    left = leftRight
    leftNode.right = this
    leftNode

  override def isHeapLeft: Boolean = left match
    case l: Node[T] => heapKey <= l.heapKey
    case _: Empty[T] => true

  override def isHeapRight: Boolean = right match
    case r: Node[T] => heapKey <= r.heapKey
    case _: Empty[T] => true

  def restoreHeapLeft(): Unit = left match
    case l: Node[T] if ! l.isHeapLeft => left = l.rotateRight
    case l: Node[T] if ! l.isHeapRight => left = l.rotateLeft
    case l: Node[T] => ()
    case l: Empty[T] => assert(false)

  def restoreHeapRight(): Unit = right match
    case r: Node[T] if ! r.isHeapLeft => right = r.rotateRight
    case r: Node[T] if ! r.isHeapRight => right = r.rotateLeft
    case r: Node[T] => ()
    case r: Empty[T] => assert(false)

class Set[T: Ordering]:
  private var root: Tree[T] = Empty()
  def isHeap: Boolean = root.isHeap
  def isBST: Boolean = root.isBST
  def isEmpty: Boolean = root.isEmpty
  def height: Int = root.height
  def size: Int = root.size
  def delete(value: T): Unit = root = root.delete(value)
  def contains(value: T): Boolean = root.contains(value)
  override def toString: String = root.toString

  def insert(value: T): Unit =
    root = root.insert(value)
    root match
      case rt: Node[T] if ! rt.isHeapLeft => root = rt.rotateRight
      case rt: Node[T] if ! rt.isHeapRight => root = rt.rotateLeft
      case rt: Node[T] => ()
      case _ : Empty[T] => assert(false)

object Week06 // Keep IntelliJ happy