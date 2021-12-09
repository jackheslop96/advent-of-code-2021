package days.day9

import days.day9.SmokeBasin._
import org.scalatest.freespec.AnyFreeSpec

class SmokeBasinSpec extends AnyFreeSpec {

  private val file = "/day-9-test-input.txt"

  "initialiseMatrix" - {
    "must return grid of all the separate values" - {
      "when given the example input" in {
        val result = initialiseMatrix(file)
        val expectedResult = Array(
          Array(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
          Array(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
          Array(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
          Array(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
          Array(9, 8, 9, 9, 9, 6, 5, 6, 7, 8)
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "run" - {
    "must return 15" - {
      "when given example input" in {
        val result = SmokeBasin.run(file)
        assert(result == 15)
      }
    }
  }
}
