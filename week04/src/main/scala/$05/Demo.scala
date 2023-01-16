package $05

import $03._
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val canvasCenter = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    val smallCircCenter = Point(ctx.canvas.width / 2, ctx.canvas.height / 2 + 0.125 * ctx.canvas.width)
    Painter(ctx) =<< Plotter(400) -<< Circle(canvasCenter, 0.375 * ctx.canvas.width, 0)
    Animator(ctx, 20) =<< Plotter(400) -<< Circle(canvasCenter, 0.25 * ctx.canvas.width, -PI)
    Animator(ctx, 20, 9000) =<< Plotter(400) -<< Circle(smallCircCenter, 0.125 * ctx.canvas.width, -3*PI/2)
  }
}
