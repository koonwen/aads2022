package $12

import $03.Point
import $05.Plotter
import $07.Polygon
import $08.Interleave
import $11.{CurveMorpher, MorphAnimator}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 10) =<< Plotter(400) =<< CurveMorpher(100) -<<
      Star(5, 2, center, 0.8 * center.x) -<< Star(8, 3, center, 0.95 * center.x)
  }
}

object Demo2 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 10) =<< Plotter(400) =<< CurveMorpher(100)-<<
      Interleave(Polygon(5, center, 0.25 * center.x), Polygon(8, center, 0.8 * center.x), 20) -<<
      Star(21, 10, center, 0.95 * center.x)
  }
}

