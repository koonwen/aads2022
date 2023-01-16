package $09

import $03._
import $06.Rotate
import $07.Concat

import Math._

case class YinYang(center: Point, radius: Length, start: Angle) extends Curve {
  def semiCircle(c: Point, r: Length): Curve = CurveSegment(Circle(c, r, 0), 0, 0.5)
  val center1: Point = Point(center.x - cos(start) * radius/2, center.y - sin(start) * radius/2)
  val center2: Point = Point(center.x + cos(start) * radius/2, center.y + sin(start) * radius/2)
  def output: Curve = Concat(
    semiCircle(center1, radius/2).rotate(center1, start + PI),
    CurveInvert(semiCircle(center2, radius/2).rotate(center2, start)),
    CurveInvert(Circle(center, radius, start)),
    semiCircle(center, radius).rotate(center, start)
  )
  def apply(t: CurveParam): Point = output(t)
}
