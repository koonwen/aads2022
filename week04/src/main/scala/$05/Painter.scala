package $05

import $03._

import org.scalajs.dom.CanvasRenderingContext2D

case class Painter(ctx: CanvasRenderingContext2D) {
  def =<< (points: Iterator[Point]): Unit = Draw(ctx, points)
}

