package $05

import $03._

case class Plotter(n: Int) {
  def -<<(curve: Curve): PointIterator = PointIterator(curve, n)
}
