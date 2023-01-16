package $04

import org.scalajs.dom.{CanvasRenderingContext2D, window}
import $03._

object Animation {
  def apply(ctx: CanvasRenderingContext2D, period: Millisecs, points: Iterator[Point]): Unit = {
    val first = points.next
    ctx.moveTo(first.x, first.y)
    ctx.beginPath()
    lazy val handle: Int = window.setInterval(() => {
      if (points.hasNext) {
        val point = points.next
        ctx.lineTo(point.x, point.y)
        ctx.stroke()
      } else {
        ctx.closePath()
        ctx.stroke()
        window.clearInterval(handle)
      }
    }, period)
    handle
  }
}
