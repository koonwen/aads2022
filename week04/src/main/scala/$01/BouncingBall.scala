package $01

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
    case AnimationState(_wipeX, _wipeY, drawX, drawY, vx, vy) =>
      val newX = drawX + vx
      val newY = drawY + vy
      val newVx = if (newX >= 25 && newX <= ctx.canvas.width - 25) vx else -vx
      val newVy = if (newY >= 25 && newY <= ctx.canvas.height - 25) vy else -vy
      AnimationState(drawX, drawY, newX, newY, newVx, newVy)
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
