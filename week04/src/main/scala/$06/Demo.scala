package $06

import $03._
import $05.{Animator, Plotter}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val start = Point(ctx.canvas.width/8,ctx. canvas.height/8)
    val end = Point(ctx.canvas.width * 7/8, ctx.canvas.height * 7/8)
    val aLine = Line(start, end)
    Animator(ctx, 10) =<< Plotter(100) -<< aLine
  }
}

object DemoRotate {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val start = Point(ctx.canvas.width/8,ctx. canvas.height/8)
    val end = Point(ctx.canvas.width * 7/8, ctx.canvas.height * 7/8)
    val aLine = Line(start, end)
    Animator(ctx, 10) =<< Plotter(100) -<< aLine
    Animator(ctx, 10, 2000) =<< Plotter(100) -<< aLine.rotate(start, PI/18)
  }
}

object DemoConcat {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val start = Point(ctx.canvas.width/8,ctx. canvas.height/8)
    val end = Point(ctx.canvas.width * 7/8, ctx.canvas.height * 7/8)
    val leftBot = Point(ctx.canvas.width/8, ctx.canvas.height * 7/8)
    val vLine = Line(start, leftBot)
    val hLine = Line(leftBot, end)
    Animator(ctx, 10) =<< Plotter(200) -<< (vLine ++ hLine)
  }
}

object DemoPolygon {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width/2, ctx.canvas.height/2)
    Animator(ctx, 5, shouldWipe = true) =<< Plotter(400) -<< PolygonNaive(3, center, 0.3 * center.x)
    Animator(ctx, 5, 3000) =<< Plotter(400) -<< PolygonNaive(4, center, 0.48 * center.x)
    Animator(ctx, 5, 6000) =<< Plotter(400) -<< PolygonNaive(5, center, 0.6 * center.x)
    Animator(ctx, 5, 9000) =<< Plotter(400) -<< PolygonNaive(6, center, 0.75 * center.x)
    Animator(ctx, 5, 12000) =<< Plotter(400) -<< PolygonNaive(7, center, 0.9 * center.x)
  }
}

object DemoLargePolygon {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width/2, ctx.canvas.height/2)
    Animator(ctx, 5) =<< Plotter(400) -<< PolygonNaive(20, center, 0.9 * center.x)
  }
}
