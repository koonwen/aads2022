package $14

import $03._

case class ProgressiveMorph(curve1: Curve, curve2: Curve, m: Double => Double) extends Curve  {
  def apply(t: CurveParam): Point =
    Point(curve1(t).x * (1-m(t)) + curve2(t).x * m(t), curve1(t).y * (1-m(t)) + curve2(t).y * m(t))
}

case class ProMorph(start: Double, end: Double) extends (Double => Double) {
  def apply(t: Double): Double = () match {
    case _ if t < start => 1
    case _ if t < end => (end - t) / (end - start)
    case _ => 0
  }
}
