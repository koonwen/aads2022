package $11

import $03.{Circle, Point}
import $05.{Painter, Plotter}
import $07.Polygon
import $08.Interleave
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 10) =<< Plotter(400) =<< CurveMorpher(100) -<< Polygon(3, center, 0.3 * center.x) -<< Polygon(7, center, 0.95 * center.x)
  }
}

object Demo2 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 10) =<< Plotter(400) =<< CurveMorpher(100) -<<
    Interleave(Circle(center, 0.6 * center.x, 0), Circle(center, 0.8 * center.x, 0), 5) -<<
      Interleave(Polygon(5, center, 0.25 * center.x), Polygon(8, center, 0.8 * center.x), 20)
  }
}