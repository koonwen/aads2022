package $12

import $03.{Curve, CurveParam, Length, Point}
import $06.Line
import $06._
import $07.Concat

import Math._

case class Star(vertices: Int, skip: Int, center: Point, radius: Length) extends Curve {
  def curve: Curve = {
    val segment: Curve = Line(
      Point(center.x + radius * cos(PI/2 + PI/vertices), center.y + radius * sin(PI/2 + PI/vertices)),
      Point(center.x + radius * cos(PI/2 - (skip * 2 - 1) * PI/vertices), center.y + radius * sin(PI/2 - (skip * 2 - 1) * PI/vertices))
    )
    Concat(Iterator.iterate(0.0, vertices)(_ - 2*skip*PI/vertices).map(segment.rotate(center, _)).toArray: _*)
  }
  def apply(t: CurveParam): Point = curve(t)
}
