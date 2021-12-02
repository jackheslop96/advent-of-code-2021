import scala.annotation.tailrec

object DepthIncreaseCounter {

  def countIncrements(depths: Seq[Int]): Int = {
    @tailrec
    def rec(ds: Seq[Int], counter: Int = 0): Int = {
      ds match {
        case Nil => counter
        case first :: next :: tail if next > first => rec(next :: tail, counter + 1)
        case _ => rec(ds.tail, counter)
      }
    }

    rec(depths)
  }
}
