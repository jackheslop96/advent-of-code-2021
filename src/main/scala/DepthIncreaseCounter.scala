import scala.annotation.tailrec

object DepthIncreaseCounter {

  def countIncrements(depths: Seq[Int]): Int = {
    countIncrements(depths, 1)
  }

  def countSlidingWindowIncrements(depths: Seq[Int]): Int = {
    countIncrements(depths, 3)
  }

  def countIncrements(depths: Seq[Int], windowSize: Int): Int = {
    @tailrec
    def rec(ds: Seq[Int], counter: Int = 0): Int = {
      ds match {
        case Nil =>
          counter
        case _ :: tail if tail.size >= windowSize =>
          val firstSum = ds.take(windowSize).sum
          val nextSum = tail.take(windowSize).sum
          if (nextSum > firstSum) {
            rec(ds.tail, counter + 1)
          } else {
            rec(ds.tail, counter)
          }
        case _ =>
          rec(ds.tail, counter)
      }
    }

    rec(depths)
  }
}
