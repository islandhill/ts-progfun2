package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      sqrt(b()) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    //composing signals without executing them
    val negativeB = Signal(-1 * b())
    val twoA = Signal(2 * a())
    val sqrtDelta = Signal(sqrt(delta()))

    Signal {
      if (delta() >= 0) {
        Set(
          (negativeB() + sqrtDelta()) / twoA(),
          (negativeB() - sqrtDelta()) / twoA()
        )
      } else {
        Set(Double.NaN, Double.NaN)
      }
    }
  }
}
