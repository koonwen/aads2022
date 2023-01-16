package $10

import $03._

case class Morph(curve1: Curve, curve2: Curve, m: MorphParam) extends Curve {
  def apply(t: CurveParam): Point = Point(curve1(t).x * (1-m) + curve2(t).x * m, curve1(t).y * (1-m) + curve2(t).y * m)
}
