package days.day06

import days.day06.LanternFish
import days.day06.LanternFish._
import org.scalatest.freespec.AnyFreeSpec

class LanternFishSpec extends AnyFreeSpec {

  private val input = "3,4,3,1,2"

  "parseInput" - {
    "must map the input to a seq of strings" in {
      val result = parseInput(Seq(input))
      val expectedResult = Seq(3, 4, 3, 1, 2)
      assertResult(expectedResult)(result)
    }
  }

  "getMap" - {
    "must map the input to a map" in {
      val result = getMap(Seq(3, 4, 3, 1, 1))
      val expectedResult = Map(1 -> 2, 3 -> 2, 4 -> 1)
      assertResult(expectedResult)(result)
    }
  }

  "simulateDay" - {
    "must move counts down a day" in {
      val input = Map(1 -> 2L, 3 -> 2L, 4 -> 1L)
      val result = simulateDay(input)
      val expectedResult = Map(0 -> 2, 2 -> 2, 3 -> 1)
      assertResult(expectedResult)(result)
    }

    "must create a 6 and an 8 from a 0" in {
      val input = Map(0 -> 1L, 7 -> 1L)
      val result = simulateDay(input)
      val expectedResult = Map(6 -> 2, 8 -> 1)
      assertResult(expectedResult)(result)
    }
  }

  "simulateDays" - {
    "must move counts down for 2 days" in {
      val input = Map(2 -> 2L, 3 -> 2L, 4 -> 1L)
      val result = simulateDays(input, 2)
      val expectedResult = Map(0 -> 2, 1 -> 2, 2 -> 1)
      assertResult(expectedResult)(result)
    }

    "must create a 6 and an 8 from a 0" in {
      val input = Map(0 -> 1L)
      val result = simulateDays(input, 2)
      val expectedResult = Map(5 -> 1, 7 -> 1)
      assertResult(expectedResult)(result)
    }
  }

  "run" - {

    val file = "/day-06-test-input.txt"

    "when 80 days" - {
      "must return 5934" in {
        val result = LanternFish.run(file, 80)
        assert(result == 5934L)
      }
    }

    "when 256 days" - {
      "must return 5934" in {
        val result = LanternFish.run(file, 256)
        assert(result == 26984457539L)
      }
    }
  }
}
