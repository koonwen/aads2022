import Ordered.orderingToOrdered
import collection.immutable.ArraySeq

sealed trait KdElem[T: Ordering]
  (val dimensions: Int, val level: Int)
    (implicit dist:(ArraySeq[T], ArraySeq[T]) => T, diff: (T, T) => T, largestValue: T):
  def isKdTree: Boolean
  def size: Int
  def search(target: ArraySeq[T]): Option[KdNode[T]]
  def insert(newPoint: ArraySeq[T]): KdNode[T]
  def findMin(idx: Int): Option[KdNode[T]]
  def remove(point: ArraySeq[T]): KdElem[T]
  def nearestNeighbor(target: ArraySeq[T]): Option[KdNode[T]]
  def nnHelper(target: ArraySeq[T], bestSoFar: Option[KdNode[T]], d: T): (Option[KdNode[T]], T)
  
case class KdLeaf[T: Ordering]
  (override val dimensions: Int, override val level: Int)
   (implicit dist:(ArraySeq[T], ArraySeq[T]) => T, diff: (T, T) => T, largestValue: T)
      extends KdElem[T](dimensions, level):
  override def isKdTree: Boolean = true
  override def size: Int = 0
  override def search(target: ArraySeq[T]): Option[KdNode[T]] = None
  override def insert(newPoint: ArraySeq[T]): KdNode[T] =
    KdNode(dimensions, level, newPoint, KdLeaf(dimensions, level+1), KdLeaf(dimensions, level+1))
  override def findMin(idx: Int): Option[KdNode[T]] = None
  override def remove(point: ArraySeq[T]): KdElem[T] = this
  override def nearestNeighbor(target: ArraySeq[T]): Option[KdNode[T]] = None
  def nnHelper(target: ArraySeq[T], bestSoFar: Option[KdNode[T]], d: T): (Option[KdNode[T]], T) = (None, largestValue)

case class KdNode[T: Ordering](
  override val dimensions: Int,
  override val level: Int,
  val coords: ArraySeq[T],
  var left: KdElem[T],
  var right: KdElem[T]
)(implicit dist:(ArraySeq[T], ArraySeq[T]) => T, diff: (T, T) => T, largestValue: T)
    extends KdElem[T](dimensions, level):

  override def isKdTree: Boolean =
    (left match {
      case l: KdNode[T] =>
        val res = coords(level % dimensions) >= l.coords(level % dimensions)
        if !res then println(s"Bad on left: $this")
        res && l.isKdTree
      case _ => true
    }) && (right match {
      case r: KdNode[T] =>
        val res = coords(level % dimensions) < r.coords(level % dimensions)
        if !res then println(s"Bad on right: $this")
        res && r.isKdTree
      case _ => true
    })

  override def size: Int = 1 + left.size + right.size

  override def search(target: ArraySeq[T]): Option[KdNode[T]] = () match {
    case _ if target == coords => Option(this)
    case _ if target(level % dimensions) < coords(level % dimensions) => left.search(target)
    case _ => right.search(target)
  }

  override def insert(newPoint: ArraySeq[T]): KdNode[T] =
    () match {
      case _ if newPoint == coords => ()
      case _ if newPoint(level % dimensions) < coords(level % dimensions) =>
        left = left.insert(newPoint)
      case _ =>
        right = right.insert(newPoint)
    }
    this

  override def findMin(idx: Int): Option[KdNode[T]] = () match {
    case _ if level % dimensions == idx && left.isInstanceOf[KdLeaf[T]] => Option(this)
    case _ if level % dimensions == idx => left.findMin(idx)
    case _ => Option(List(Option(this), left.findMin(idx), right.findMin(idx)).flatten.minBy(_.coords(idx)))
  }

  override def remove(point: ArraySeq[T]): KdElem[T] = () match {
    case _ if point == coords && right.isInstanceOf[KdNode[T]] => // this
      val minNode = right.findMin(level % dimensions).get
      val newRight = right.remove(minNode.coords)
      KdNode(dimensions, level, minNode.coords, left, newRight)
    case _ if point == coords && left.isInstanceOf[KdNode[T]] =>
      val minNode = left.findMin(level % dimensions).get
      val newRight = left.remove(minNode.coords)
      KdNode(dimensions, level, minNode.coords, KdLeaf(dimensions, level+1), newRight)
    case _ if point == coords =>
      KdLeaf(dimensions, level)
    case _ if point(level % dimensions) <= coords(level % dimensions) =>
      left = left.remove(point)
      this
    case _ =>
      right = right.remove(point)
      this
  }

  override def nearestNeighbor(target: ArraySeq[T]): Option[KdNode[T]] = nnHelper(target, None, largestValue)._1

  def nnHelper(target: ArraySeq[T], bestSoFar: Option[KdNode[T]], d: T): (Option[KdNode[T]], T) = {
    val currDist = dist(coords, target)
    val (near, far) = if target(level % dimensions)  < coords(level % dimensions) then (left, right) else (right, left)
    val (bbNode, bbDist) = if currDist < d then (Option(this), currDist) else (bestSoFar, d)
    val (nearNode, nearDist) = near.nnHelper(target, bbNode, bbDist)
    val (farNode, farDist) = if diff(target(level % dimensions), coords(level % dimensions)) < bbDist
                             then far.nnHelper(target, bbNode, bbDist)
                             else (None, largestValue)
    Seq((bbNode, bbDist), (nearNode, nearDist), (farNode, farDist)).minBy(_._2)
  }

case class KdTree[T: Ordering](private var root: KdElem[T], val dimensions: Int):
  def isKdTree: Boolean = root.isKdTree
  def size: Int = root.size
  def search(target: ArraySeq[T]): Option[KdNode[T]] = root.search(target)
  def insert(point: ArraySeq[T]): Unit = root = root.insert(point)
  def findMin(idx: Int) = root.findMin(idx)
  def remove(point: ArraySeq[T]): Unit = root = root.remove(point)
  def nearestNeighbor(target: ArraySeq[T]): Option[ArraySeq[T]] = root.nearestNeighbor(target).map(_.coords)


object KdTree:
  def partition[T: Ordering](dimensions: Int, level: Int, points: Array[ArraySeq[T]], low: Int, high: Int): Int =
    val mid = Iterator.iterate((low+1, low+1), high-low) {
      case (mid, top) if points(top)(level % dimensions) > points(low)(level % dimensions) => (mid, top+1)
      case (mid, top) =>
        val aux = points(top)
        points(top) = points(mid)
        points(mid) = aux
        (mid+1, top+1)
    }.drop(high-low-1).nextOption.map(_._1 - 1)
    mid.foreach { m =>
      val aux = points(low)
      points(low) = points(m)
      points(m) = aux
    }
    mid.getOrElse(low)
  
  enum Link:
    case Root, Left, Right
  import Link._

  def apply[T: Ordering]
    (dimensions: Int, points: Array[ArraySeq[T]])
      (implicit dist:(ArraySeq[T], ArraySeq[T]) => T, diff: (T, T) => T, largestValue: T): KdTree[T] =
    var treeBuild: KdElem[T] = KdLeaf(dimensions, 0)
    Iterator.iterate(List((0, points.length, 0, treeBuild, Root))) {
      case (low, high, level, node, link) :: tail if low < high =>
        val pivotIndex = partition(dimensions, level, points, low, high)
        val newTreeNode = KdNode(dimensions, level, points(pivotIndex), KdLeaf(dimensions, level + 1), KdLeaf(dimensions, level + 1))
        link match {
          case Root => treeBuild = newTreeNode
          case Left => node.asInstanceOf[KdNode[T]].left = newTreeNode
          case Right => node.asInstanceOf[KdNode[T]].right = newTreeNode
        }
        (low, pivotIndex, level + 1, newTreeNode, Left) :: (pivotIndex + 1, high, level + 1, newTreeNode, Right) :: tail
      case _ :: tail => tail
    }.dropWhile(_.nonEmpty).next
    new KdTree(treeBuild, dimensions)
  
