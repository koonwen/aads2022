package $13

import $03.Curve
import $10.Morph

case class CycleMorpher(n: Int) {
  def -<< (curves: Array[Curve]): Iterator[Curve] = {
    Iterator.iterate((0, Iterator.empty[Curve])) {
      case (i, _) =>
        val newI = (i + 1) % curves.length
        val morphedCurves = (0 to n).map(k => Morph(curves(i), curves(newI), k.toDouble/n))
        val pause = (0 to n).map(_ => curves(newI))
        (newI, (morphedCurves ++ pause).iterator)
    }.flatMap(_._2)
  }
}
