package $09

import $03._
import $05.{Animator, Plotter}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    Animator(ctx, 5) =<< Plotter(400) -<<
      YinYang(center, 0.8 * center.x, 0)
    Animator(ctx, 5, 3000, shouldWipe = true) =<< Plotter(400) -<<
      YinYang(center, 0.8 * center.x, PI / 4)
    Animator(ctx, 5, 6000, shouldWipe = true) =<< Plotter(400) -<<
      YinYang(center, 0.8 * center.x, - PI / 4)
  }
}
