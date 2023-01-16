package $14

import $03.{Circle, Curve, Point}
import $05.Plotter
import $07.{Concat, Polygon}
import $08.Interleave
import $11.{CurveMorpher, MorphAnimator}
import $12.Star
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent}
import $06._
import $09.CurveSegment
import $13.MorphAnimCycle

import Math._

object Demo {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 20) =<< Plotter(400) =<< ProCurveMorpher(100) -<<
      Polygon(5, center, 0.8 * ctx.canvas.width / 2) -<< Polygon(7, center, 0.8 * ctx.canvas.width / 2 )
  }
}

object Demo2 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 20) =<< Plotter(400) =<< ProCurveMorpher(100) -<<
      Star(5, 2, center, 0.8 * ctx.canvas.width / 2) -<< Star(21, 10, center, 0.8 * ctx.canvas.width / 2 )
  }
}

object Demo3 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 20) =<< Plotter(400) =<< ProCurveMorpher(100) -<<
      Polygon(5, center, 0.8 * ctx.canvas.width / 2) -<< Star(5, 2, center, 0.8 * ctx.canvas.width / 2 )
  }
}

object Demo4 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val center = Point(ctx.canvas.width / 2, ctx.canvas.height / 2)
    MorphAnimator(ctx, 20) =<< Plotter(400) =<< ProCurveMorpher(100) -<<
      Interleave(Circle(center, 0.6 * center.x, 0), Circle(center, 0.8 * center.x, 0), 5) -<<
      Interleave(Polygon(5, center, 0.25 * center.x), Polygon(8, center, 0.8 * center.x), 6).rotate(center, - Math.PI / 2)
  }
}

object Demo5 {
  def apply(ctx: CanvasRenderingContext2D): UIEvent => Unit = _ => {
    val cc = Point(ctx.canvas.width/2, ctx.canvas.height/2) // Canvas center
    val rBig = cc.x * 3 / 4  // big radius
    val rMed = cc.x / 2      // medium radius
    val rSma = cc.x / 4      // small radius
    val circBig = Circle(cc, rBig, 0)
    val circMed = Circle(cc, rMed, 0)

    val curves = Array[Curve](
      Interleave(circBig, circMed, 50),
      Interleave(Polygon(12, cc, rBig), Polygon(6, cc, rSma).rotate(cc, PI/6), 20),
      Star(21, 2, cc, rBig),
      Interleave(Circle(cc, rBig, PI/3), Circle(cc, rMed, PI/3), 12),
      Interleave(circBig, circMed, 16),
      Star(21, 10, cc, rBig),
      Star(5, 2, cc, rBig),
      Interleave(circBig, circMed, 48),
      Interleave(circBig, circMed, 44),
      Interleave(circBig, circMed, 40),
      Interleave(circBig, circMed, 36),
      Interleave(circBig, circMed, 32),
      Interleave(circBig, circMed, 28),
      Interleave(circBig, circMed, 24),
      Interleave(circBig, circMed, 20),
      Interleave(circBig, circMed, 16),
      Interleave(circBig, circMed, 12),
      Interleave(circBig, circMed, 8),
      Circle(cc, cc.x, PI/2),
      Concat(
        CurveSegment(circBig, 0, 0.5),
        Circle(Point(cc.x - rBig/2, cc.y), rBig/2, PI),
        CurveSegment(circBig, 0.5, 1),
        Circle(Point(cc.x + rBig/2, cc.y), rBig/2, 0)
      ),
      Interleave(Polygon(4, Point(cc.x + rSma, cc.y + rSma), rSma), Polygon(5, cc, rBig), 50),
      Interleave(Polygon(3, Point(cc.x - rSma, cc.y - rSma), rSma), Polygon(7, cc, rBig), 50),
      Concat(
        CurveSegment(Circle(cc, rBig, PI/2), 0, 0.5),
        Circle(Point(cc.x, cc.y - rBig/2), rBig/2, -PI/2),
        CurveSegment(Circle(cc, rBig, PI/2), 0.5, 1),
        Circle(Point(cc.x, cc.y + rBig/2), rBig/2, PI/2)
      ),
      Concat(
        CurveSegment(Circle(cc, rMed, -3*PI/2), 0, 0.5),
        Circle(Point(cc.x, cc.y - rSma), rSma, -PI/2),
        CurveSegment(Circle(cc, rMed, PI/2), 0.5, 1),
        Circle(Point(cc.x, cc.y + rSma), rSma, PI/2)
      ),
      Interleave(
        Concat(
          CurveSegment(Circle(cc, rBig, PI/2), 0, 0.5),
          Circle(Point(cc.x, cc.y - rBig/2), rBig/2, -PI/2),
          CurveSegment(Circle(cc, rBig, -3*PI/2), 0.5, 1),
          Circle(Point(cc.x, cc.y + rBig/2), rBig/2, PI/2)
        ),
        Concat(
          CurveSegment(Circle(cc, rMed, PI/2), 0, 0.5),
          Circle(Point(cc.x, cc.y - rSma), rSma, -PI/2),
          CurveSegment(Circle(cc, rMed, PI/2), 0.5, 1),
          Circle(Point(cc.x, cc.y + rSma), rSma, PI/2)
        ),
        50),
    )
    MorphAnimCycle(ctx, 10) =<< Plotter(400) =<< ProCycleMorpher(100) -<< curves
  }

}

