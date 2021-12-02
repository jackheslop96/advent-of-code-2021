import scala.annotation.tailrec

object DepthIncreaseCounter {

  def countIncrements(depths: Seq[Int], windowSize: Int = 1): Int = {

    def sumOfFirstWindow(xs: Seq[Int]): Int = xs.take(windowSize).sum

    @tailrec
    def rec(ds: Seq[Int], counter: Int = 0): Int = {
      ds match {
        case Nil =>
          counter
        case _ :: tail if tail.size >= windowSize =>
          val firstSum = sumOfFirstWindow(ds)
          val nextSum = sumOfFirstWindow(tail)
          if (nextSum > firstSum) {
            rec(tail, counter + 1)
          } else {
            rec(tail, counter)
          }
        case _ =>
          rec(ds.tail, counter)
      }
    }

    rec(depths)
  }
}
