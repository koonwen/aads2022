package $10

import $03._
import $05.{Animator, Plotter}
import $07.Polygon
import Math._
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    Animator(ctx, 5) =<< Plotter(400) -<<
      Morph(Polygon(4, center, 0.25 * center.x), Circle(center, 0.75 * center.x, PI / 4), 0.5)
    Animator(ctx, 5, 3000) =<< Plotter(400) -<< Polygon(4, center, 0.25 * center.x)
    Animator(ctx, 5, 6000) =<< Plotter(400) -<< Circle(center, 0.75 * center.x, PI / 4)
    Animator(ctx, 5, 9000, shouldWipe = true) =<< Plotter(400) -<<
      Morph(Polygon(4, center, 0.25 * center.x), Circle(center, 0.75 * center.x, 0), 0.5)
    Animator(ctx, 5, 12000) =<< Plotter(400) -<< Polygon(4, center, 0.25 * center.x)
    Animator(ctx, 5, 15000) =<< Plotter(400) -<< Circle(center, 0.75 * center.x, 0)
    Animator(ctx, 5, 18000, shouldWipe = true) =<< Plotter(400) -<<
      Morph(Polygon(4, center, 0.25 * center.x), Circle(center, 0.75 * center.x, PI), 0.5)
    Animator(ctx, 5, 21000) =<< Plotter(400) -<< Polygon(4, center, 0.25 * center.x)
    Animator(ctx, 5, 24000) =<< Plotter(400) -<< Circle(center, 0.75 * center.x, PI)
  }
}
