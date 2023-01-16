package $00

import org.scalajs.dom.{document, window}
import org.scalajs.dom.html.{Div, Button}

object CreatePanelLeft {
  def apply(buttons: List[Button]): Div = {
    val panelLeft = document.createElement("div").asInstanceOf[Div]
    panelLeft.style.width = "150px"
    panelLeft.style.height = s"${window.innerHeight + 220}px"
    panelLeft.style.position = "absolute"
    panelLeft.style.display = "block"
    panelLeft.style.left = "0px"
    panelLeft.style.top = "0px"
    panelLeft.style.border = "1px black solid"
    document.body.appendChild(panelLeft)
    buttons.foreach(panelLeft.appendChild)
    panelLeft
  }
}
