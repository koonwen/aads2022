package $00

import org.scalajs.dom
import org.scalajs.dom.html.{Button, Canvas, Div}
import org.scalajs.dom.{CanvasRenderingContext2D, UIEvent, document, window}

object CreateButton {
  def apply(text: String, action: UIEvent => Unit): Button = {
    val button = document.createElement("button").asInstanceOf[Button]
    button.style.margin = "15px 25px -10px 25px"
    button.style.padding = "auto"
    button.style.padding = "7px"
    button.style.border = "1px red solid"
    button.style.width = "100px"
    button.style.height = "30px"
    button.`type` = "button"
    button.innerText = text
    button.addEventListener("click", action)
    button
  }
}
