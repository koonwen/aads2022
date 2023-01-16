package $03

import org.scalajs.dom.CanvasRenderingContext2D

object Draw {
  def apply(ctx: CanvasRenderingContext2D, points: Iterator[Point]): Unit = {
    val first = points.next
    ctx.moveTo(first.x, first.y)
    ctx.beginPath()
    points.foreach(point => ctx.lineTo(point.x, point.y))
    ctx.closePath()
    ctx.stroke()
  }
}
