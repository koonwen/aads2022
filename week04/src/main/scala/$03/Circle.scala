package $03
import Math._

case class Circle(center: Point, radius: Length, startAngle: Angle) extends Curve {
  def apply(t: CurveParam): Point =
    Point(center.x + radius * cos(startAngle + t * 2 * PI), center.y + radius * sin(startAngle + t * 2 * PI))
}
