package $08

import $03._

case class Interleave(curve1: Curve, curve2: Curve, intervals: Int) extends Curve {
  def apply(t: CurveParam): Point = {
    val k = (2 * t * intervals).toInt
    if (k % 2 == 0)
      curve1(t)
    else
      curve2(t)
  }
}
