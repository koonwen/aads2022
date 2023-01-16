import $00._
import org.scalajs.dom.UIEvent

object Week04 {

  def main(args: Array[String]): Unit = {
    val ctx = SetUpCanvas(800, 50)
    val buttons = List(
      CreateButton("Demo 01", $01.BouncingBall(ctx)),
      CreateButton("Demo 02", $02.BouncingBall(ctx)),
      CreateButton("Demo 03", $03.Demo(ctx)),
      CreateButton("Demo 04", $04.Demo(ctx)),
      CreateButton("Demo 05", $05.Demo(ctx)),
      CreateButton("Demo 06", $06.Demo(ctx)),
      CreateButton("Demo 07", $06.DemoRotate(ctx)),
      CreateButton("Demo 08", $06.DemoConcat(ctx)),
      CreateButton("Demo 09", $06.DemoPolygon(ctx)),
      CreateButton("Demo 10", $06.DemoLargePolygon(ctx)),
      CreateButton("Demo 11", $07.Demo(ctx)),
      CreateButton("Demo 12", $08.Demo(ctx)),
      CreateButton("Demo 13", $09.Demo(ctx)),
      CreateButton("Demo 14", $10.Demo(ctx)),
      CreateButton("Demo 15", $11.Demo(ctx)),
      CreateButton("Demo 16", $11.Demo2(ctx)),
      CreateButton("Demo 17", $12.Demo(ctx)),
      CreateButton("Demo 18", $12.Demo2(ctx)),
      CreateButton("Demo 19", $13.Demo(ctx)),
      CreateButton("Demo 20", $13.Demo2(ctx)),
      CreateButton("Demo 21", $14.Demo(ctx)),
      CreateButton("Demo 22", $14.Demo2(ctx)),
      CreateButton("Demo 23", $14.Demo3(ctx)),
      CreateButton("Demo 24", $14.Demo4(ctx)),
      CreateButton("Demo 25", $14.Demo5(ctx)),
      CreateButton("Demo 26", (_:UIEvent) => ???),
      CreateButton("Demo 27", (_:UIEvent) => ???),
      CreateButton("Demo 28", (_:UIEvent) => ???),
      )
    CreatePanelLeft(buttons)
  }
}