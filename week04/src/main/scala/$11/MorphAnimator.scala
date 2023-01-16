package $11

import $03._
import $05._
import org.scalajs.dom.{CanvasRenderingContext2D, window}

case class MorphAnimator(ctx: CanvasRenderingContext2D, period: Millisecs) {
  def =<< (p: Plotter): MorphAnimatorWithPainter = MorphAnimatorWithPainter(p, Painter(ctx))

  case class MorphAnimatorWithPainter(p: Plotter, painter: Painter) {
    def =<< (ci: Iterator[Curve]): Unit = {
      val a = ci.map(curve => (p -<< curve).toList).toList
      val ar = a.reverse
      val it = (a ++ List.fill(a.length)(ar.head) ++ ar).map(_.iterator).iterator
      lazy val handle: Int = window.setInterval( () => {
        if(it.hasNext) {
          ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
          painter =<< it.next
        } else {
          window.clearInterval(handle)
        }
      }, period)
      handle
    }
  }
}
