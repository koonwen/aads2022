package $00

import org.scalajs.dom
import org.scalajs.dom.html.{Button, Canvas, Div}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent, document, window}

object SetUpCanvas {
  def apply(size: Int, topMargin: Int): dom.CanvasRenderingContext2D = {

    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.style.width = s"${size}px"
    canvas.style.height = s"${size}px"
    canvas.style.left = "150px"
    canvas.style.top = "0px"
    canvas.style.position = "absolute"
    canvas.style.display = "block"
    canvas.style.background = "#f8f8ff"
    canvas.style.margin = s"${topMargin}px ${(window.innerWidth - size - 150) / 2}px"
    canvas.style.border = "solid 1px #eeeeee"
    canvas.width = size
    canvas.height = size
    document.body.appendChild(canvas)
    dom.window.addEventListener(
      "resize",
      (e: UIEvent) => canvas.style.margin = s"${topMargin}px ${(window.innerWidth - size - 150) / 2}px"
      )
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.lineWidth = 8.0
    ctx
  }

}
