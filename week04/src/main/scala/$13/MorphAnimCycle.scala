package $13

import $03.{Curve, Millisecs}
import $05.{Painter, Plotter}
import org.scalajs.dom.{CanvasRenderingContext2D, window}

case class MorphAnimCycle(ctx: CanvasRenderingContext2D, period: Millisecs) {
  def =<< (p: Plotter): MorphAnimatorWithPainter = MorphAnimatorWithPainter(p, Painter(ctx))

  case class MorphAnimatorWithPainter(p: Plotter, painter: Painter) {
    def =<< (ci: Iterator[Curve]): Unit = {
      val pi = ci.map(p -<< _)
      lazy val handle: Int = window.setInterval( () => {
        if(pi.hasNext) {
          ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
          painter =<< pi.next
        } else {
          window.clearInterval(handle)
        }
      }, period)
      handle
    }
  }
}
