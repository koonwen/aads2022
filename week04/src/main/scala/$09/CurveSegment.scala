package $09

import $03._

case class CurveSegment(curve: Curve, start: CurveParam, end: CurveParam) extends Curve {
  def output: Curve = t => curve(start * (1-t) + end * t)
  def apply(t: CurveParam): Point = output(t)
}
