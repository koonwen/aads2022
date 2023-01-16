package $02

import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent, window}

import scala.util.Random

case class BouncingBall(ctx: CanvasRenderingContext2D) extends (UIEvent => Unit) {

  def apply(e: UIEvent): Unit = window.setInterval(() => renderFrame(flow.next), 10)

  case class AnimationState(
    wipeX: Double = 0.0,
    wipeY: Double = 0.0,
    drawX: Double = ctx.canvas.width / 2.0,
    drawY: Double = ctx.canvas.height / 2.0,
    vx: Double = Random.nextDouble() * 10,
    vy: Double = Random.nextDouble() * 10
  )

  val flow: Iterator[AnimationState] = Iterator.iterate(AnimationState()) {
    case AnimationState(wipeX, wipeY, drawX, drawY, vx, vy) =>
      val rawX = drawX + vx
      val rawY = drawY + vy
      val rawVx = if (rawX >= 25 && rawX <= ctx.canvas.width - 25 && Math.abs(wipeX - drawX) > 1) vx
                  else -vx + Random.nextDouble() * 6 - 3.0
      val rawVy = if (rawY >= 25 && rawY <= ctx.canvas.height - 25 && Math.abs(wipeY - drawY) > 1) vy
                  else -vy + Random.nextDouble() * 6 - 3.0
      val x = if (rawX < 26) 26 else if (rawX > ctx.canvas.width - 26) ctx.canvas.width - 26 else rawX
      val y = if (rawY < 26) 26 else if (rawY > ctx.canvas.height - 26) ctx.canvas.height - 26 else rawY

      val newVx = if (Math.abs(rawVx) > 10) rawVx * 0.8 else if (Math.abs(rawVx) < 1.0) 1.0 else rawVx
      val newVy = if (Math.abs(rawVy) > 10) rawVy * 0.8 else if (Math.abs(rawVy) < 1.0) 1.0 else rawVy
      AnimationState(drawX, drawY, x, y, newVx, newVy)
  }

  def renderFrame: AnimationState => Unit = {
    case AnimationState(wipeX, wipeY, drawX, drawY, _, _) =>
      ctx.clearRect(wipeX - 30, wipeY - 30, 60, 60)
      drawBall(drawX, drawY, "lightblue", "red")
  }


  def drawBall(x: Double, y: Double, colorStroke: String, colorFill: String): Unit = {
    ctx.fillStyle = colorFill
    ctx.strokeStyle = colorStroke
    ctx.lineWidth = 5
    ctx.beginPath()
    ctx.ellipse(x, y, 25, 25, 0, 0, 2 * Math.PI)
    ctx.stroke()
    ctx.fill()
  }
}
