package $06

import $03._

case class Line(start: Point, end: Point) extends Curve {
  def apply(t: CurveParam): Point = Point(start.x * (1-t) + end.x * t, start.y * (1-t) + end.y * t)
}
