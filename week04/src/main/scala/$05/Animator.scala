package $05

import $03._
import $04.Animation
import org.scalajs.dom.{CanvasRenderingContext2D, window}

case class Animator(ctx: CanvasRenderingContext2D, period: Millisecs, delay: Millisecs = 0, shouldWipe: Boolean = false) {
  def =<< (points: Iterator[Point]): Unit = window.setTimeout(() => {
      if(shouldWipe) ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
      Animation(ctx, period, points)
    },
    delay
  )
}
