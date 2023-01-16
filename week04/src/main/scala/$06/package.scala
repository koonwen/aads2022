import Math._
import $03._

package object $06 {
  implicit class Rotate(curve: Curve) {
    def rotate(center: Point, angle: Angle): Curve = t => {
      val Point(x, y) = curve(t)
      val (dx, dy) = (x - center.x, y - center.y)
      val (dxR, dyR) = (dx * cos(angle) - dy * sin(angle), dx * sin(angle) + dy * cos(angle))
      Point(center.x + dxR, center.y + dyR)
    }
  }

  implicit class Append(curve: Curve) {
    def ++ (other: Curve): Curve = {
      case t if t < 0.5 => curve(2 * t)
      case t => other(2 * t - 1)
    }
  }
}
