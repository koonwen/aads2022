package $03

case class PointIterator(curve: Curve, n: Int) extends Iterator[Point] {
  private val it = Iterator.iterate((curve(0), 1), n){ case (_, k) => (curve(k.toDouble/n), k+1)}
  override def hasNext: Boolean = it.hasNext
  override def next(): Point = it. next._1
}
