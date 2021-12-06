package day1

import days.day1.DepthIncreaseCounter
import days.day1.DepthIncreaseCounter._
import org.scalatest.freespec.AnyFreeSpec

class DepthIncreaseCounterSpec extends AnyFreeSpec {

  "countIncrements" - {

    "when sliding window size is 1" - {

      val windowSize = 1

      "must return 0 for empty list of depths" in {
        val depths = Nil
        val result = countIncrements(depths, windowSize)
        assert(result == 0)
      }

      "must return 0 for list of 1 depth" in {
        val depths = Seq(1)
        val result = countIncrements(depths, windowSize)
        assert(result == 0)
      }

      "must return 0 for multiple values with decreasing depth" in {
        val depths = Seq(3, 2, 1)
        val result = countIncrements(depths, windowSize)
        assert(result == 0)
      }

      "must return 0 for multiple values with constant depth" in {
        val depths = Seq(2, 2, 2)
        val result = countIncrements(depths, windowSize)
        assert(result == 0)
      }

      "must return 2 for 3 values with increasing depth" in {
        val depths = Seq(1, 2, 3)
        val result = countIncrements(depths, windowSize)
        assert(result == 2)
      }

      "must return 7 for example input" in {
        val result = DepthIncreaseCounter.run("/day-1-test-input.txt", windowSize)
        assert(result == 7)
      }
    }

    "when sliding window size is 3" - {

      val windowSize = 3

      "must return 5 for example input" in {
        val result = DepthIncreaseCounter.run("/day-1-test-input.txt", windowSize)
        assert(result == 5)
      }
    }
  }
}
