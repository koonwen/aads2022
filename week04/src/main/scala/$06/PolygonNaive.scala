package $06

import Math._
import $03._


case class PolygonNaive(sides: Int, center: Point, radius: Length) extends Curve {
  def curve: Curve = {
    val start = Point(center.x + radius * cos(PI/2 + PI/sides), center.y + radius * sin(PI/2 + PI/sides))
    val end = Point(center.x + radius * cos(PI/2 - PI/sides), center.y + radius * sin(PI/2 - PI/sides))
    val lowerHorizontalSide: Curve = Line(start, end)
    (1 until sides).map(- _ * 2 * PI / sides).foldLeft(lowerHorizontalSide){
      case (curve, ang) => curve ++ lowerHorizontalSide.rotate(center, ang)
    }
  }
  def apply(t: CurveParam): Point = curve(t)
}
