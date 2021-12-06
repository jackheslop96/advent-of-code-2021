package fish

import fish.LanternFish._
import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader.stringFileReader

class LanternFishSpec extends AnyFreeSpec {

  private val input = "3,4,3,1,2"

  "getListOfFish" - {
    "must return list of fish" - {
      "when given input string" in {
        val result = getListOfFish(input)
        val expectedResult = Seq(
          LanternFish(3),
          LanternFish(4),
          LanternFish(3),
          LanternFish(1),
          LanternFish(2)
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "simulateDay" - {
    "must minus 1 from each lantern fish" in {
      val lanternFish = Seq(
        LanternFish(3)
      )
      val result = simulateDay(lanternFish)
      val expectedResult = Seq(
        LanternFish(2)
      )
      assertResult(expectedResult)(result)
    }

    "must append new lantern fish if one reaches zero and reset 0 to 6" in {
      val lanternFish = Seq(
        LanternFish(0)
      )
      val result = simulateDay(lanternFish)
      val expectedResult = Seq(
        LanternFish(6),
        LanternFish(8)
      )
      assertResult(expectedResult)(result)
    }
  }

  "simulateNDays" - {
    "must simulate days" - {
      "when 2 days" in {
        val lanternFish = Seq(
          LanternFish(0)
        )
        val result = simulateNDays(lanternFish, 2)
        val expectedResult = Seq(
          LanternFish(5),
          LanternFish(7)
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "modelPopulationGrowth" - {
    "must return the number of fish after a given number of days" - {
      "when given 80 days" in {
        val input = stringFileReader("/day-6-test-input.txt")
        val result = modelPopulationGrowth(input, 80)
        assert(result == 5934)
      }
    }
  }
}
