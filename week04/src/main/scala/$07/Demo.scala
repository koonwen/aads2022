package $07

import $03._

import $05.{Animator, Plotter}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width/2, ctx.canvas.height/2)
    Animator(ctx, 5, shouldWipe = true) =<< Plotter(400) -<< Polygon(3, center, 0.15 * center.x)
    Animator(ctx, 5, 3000) =<< Plotter(400) -<< Polygon(4, center, 0.27 * center.x)
    Animator(ctx, 5, 6000) =<< Plotter(400) -<< Polygon(5, center, 0.37 * center.x)
    Animator(ctx, 5, 9000) =<< Plotter(400) -<< Polygon(6, center, 0.5 * center.x)
    Animator(ctx, 5, 12000) =<< Plotter(400) -<< Polygon(7, center, 0.63 * center.x)
    Animator(ctx, 5, 15000) =<< Plotter(400) -<< Polygon(8, center, 0.7 * center.x)
    Animator(ctx, 5, 18000) =<< Plotter(400) -<< Polygon(9, center, 0.77 * center.x)
    Animator(ctx, 5, 21000) =<< Plotter(400) -<< Polygon(10, center, 0.85 * center.x)
    Animator(ctx, 5, 24000) =<< Plotter(400) -<< Polygon(20, center, 0.95 * center.x)
  }
}
