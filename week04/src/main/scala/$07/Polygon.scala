package $07

import $03._
import $06.Line
import Math._
import $06.{Append, Rotate}

case class Polygon(sides: Int, center: Point, radius: Length) extends Curve {
  def curve: Curve = {
    val start = Point(center.x + radius * cos(PI/2 - PI/sides), center.y + radius * sin(PI/2 - PI/sides))
    val end = Point(center.x + radius * cos(PI/2 + PI/sides), center.y + radius * sin(PI/2 + PI/sides))
    val lowerHorizontalSide: Curve = Line(start, end)
    Concat((0 until sides).map(k => lowerHorizontalSide.rotate(center, k * 2 * PI / sides)): _*)
  }
  def apply(t: CurveParam): Point = curve(t)
}
