package $09

import $03._

case class CurveInvert(curve: Curve) extends Curve {
  def output: Curve = (1 - (_: CurveParam)) andThen curve
  def apply(t: CurveParam): Point = output(t)
}
