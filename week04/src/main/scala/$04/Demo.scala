package $04

import $03._

import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ =>
    Animation(ctx, 20, PointIterator(Circle(Point(ctx.canvas.width / 2, ctx.canvas.height / 2), 0.375 * ctx.canvas.width, PI/2), 400))
}
