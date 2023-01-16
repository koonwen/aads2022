package $14

import $03.Curve

case class ProCurveMorpher(n: Int) {
  def -<< (curve: Curve): CurveMorpherWith1Curve = CurveMorpherWith1Curve(n, curve)
  case class CurveMorpherWith1Curve(n: Int, curve1: Curve) {
    def -<< (curve2: Curve): Iterator[Curve] = {
      (0 to n).map(k => ProgressiveMorph(curve1, curve2, ProMorph(1.1 * k.toDouble/n - 0.1, k.toDouble/n))).iterator
    }
  }

}
