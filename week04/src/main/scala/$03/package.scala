package object $03 {
  type CurveParam = Double
  type Angle = Double
  type Length = Double
  type Coord = Double
  type Millisecs = Double
  case class Point(x: Coord, y: Coord)
  type Curve = CurveParam => Point
}
