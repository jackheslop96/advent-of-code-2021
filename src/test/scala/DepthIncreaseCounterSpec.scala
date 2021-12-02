import DepthIncreaseCounter.countIncrements
import org.scalatest.freespec.AnyFreeSpec

class DepthIncreaseCounterSpec extends AnyFreeSpec {

  "countIncrements" - {

    "when window size is 1" - {

      "must return 0 for empty list of depths" in {
        val depths = Nil
        val result = countIncrements(depths)
        assert(result == 0)
      }

      "must return 0 for list of 1 depth" in {
        val depths = Seq(1)
        val result = countIncrements(depths)
        assert(result == 0)
      }

      "must return 0 for multiple values with decreasing depth" in {
        val depths = Seq(3, 2, 1)
        val result = countIncrements(depths)
        assert(result == 0)
      }

      "must return 0 for multiple values with constant depth" in {
        val depths = Seq(2, 2, 2)
        val result = countIncrements(depths)
        assert(result == 0)
      }

      "must return 2 for 3 values with increasing depth" in {
        val depths = Seq(1, 2, 3)
        val result = countIncrements(depths)
        assert(result == 2)
      }

      "must return 7 for Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)" in {
        val depths = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
        val result = countIncrements(depths)
        assert(result == 7)
      }
    }

    "when window size is 3" - {
      "must return 5 for Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)" in {
        val depths = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
        val result = countIncrements(depths, 3)
        assert(result == 5)
      }
    }
  }
}
