package $03
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ =>
    Draw(ctx, PointIterator(Circle(Point(ctx.canvas.width / 2, ctx.canvas.height / 2), 0.375 * ctx.canvas.width, 0), 400))
}
