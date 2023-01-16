package $07

import $03._

object Concat {
  def apply(curves : Curve*): Curve = {
    val arr = curves.toArray
    t => {
      val k = (t * curves.length).toInt
      arr(k)(t*curves.length - k)
    }
  }
}
