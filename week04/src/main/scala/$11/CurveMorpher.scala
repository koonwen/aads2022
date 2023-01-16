package $11

import $03.Curve
import $10.Morph

case class CurveMorpher(n: Int) {
  def -<< (curve: Curve): CurveMorpherWith1Curve = CurveMorpherWith1Curve(n, curve)
  case class CurveMorpherWith1Curve(n: Int, curve1: Curve) {
    def -<< (curve2: Curve): Iterator[Curve] = {
      (0 to n).map(k => Morph(curve1, curve2, k.toDouble/n)).iterator
    }
  }
}
