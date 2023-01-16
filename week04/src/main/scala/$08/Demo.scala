package $08

import $03._

import $05.{Animator, Plotter}
import $07.Polygon
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    Animator(ctx, 5, shouldWipe = true) =<< Plotter(400) -<<
      Interleave(Circle(center, 0.5 * center.x, 0), Circle(center, 0.8 * center.x, 0), 5)
    Animator(ctx, 5, 3000, shouldWipe = true) =<< Plotter(400) -<<
      Interleave(Polygon(3, center, 0.25 * center.x), Circle(center, 0.9 * center.x, PI/6), 3)
    Animator(ctx, 5, 6000, shouldWipe = true) =<< Plotter(400) -<<
      Interleave(Circle(center, 0.6 * center.x, 0), Circle(center, 0.8 * center.x, 0), 50)
    Animator(ctx, 5, 9000, shouldWipe = true) =<< Plotter(400) -<<
      Interleave(Polygon(5, center, 0.25 * center.x), Polygon(8, center, 0.8 * center.x), 20)

  }
}
