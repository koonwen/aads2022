package $14
import $03.Curve
import $10.Morph

case class ProCycleMorpher(n: Int) {
  def -<< (curves: Array[Curve]): Iterator[Curve] = {
    Iterator.iterate((0, Iterator.empty[Curve])) {
      case (i, _) =>
        val newI = (i + 1) % curves.length
        val morphedCurves =
          (0 to n)
            .map(k => ProgressiveMorph(curves(i), curves(newI), ProMorph(1.1 * k.toDouble/n - 0.1, k.toDouble/n)))
        val pause = (0 to n).map(_ => curves(newI))
        (newI, (morphedCurves ++ pause).iterator)
    }.flatMap(_._2)
  }
}
