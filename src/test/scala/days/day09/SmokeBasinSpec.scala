package days.day09

import days.day09.SmokeBasin._
import org.scalatest.freespec.AnyFreeSpec

class SmokeBasinSpec extends AnyFreeSpec {

  private val file = "/day-9-test-input.txt"

  private val matrix: Matrix = Array(
    Array(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
    Array(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
    Array(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
    Array(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
    Array(9, 8, 9, 9, 9, 6, 5, 6, 7, 8)
  )

  "initialiseMatrix" - {
    "must return grid of all the separate values" - {
      "when given the example input" in {
        val result = initialiseMatrix(file)
        assertResult(matrix)(result)
      }
    }
  }

  "part1" - {
    "must return 15" - {
      "when given example input" in {
        val result = SmokeBasin.part1(file)
        assert(result == 15)
      }
    }
  }

  "calculateSizeOfBasin" - {
    "must return 3 for top-left basin in example input" in {
      val lowPoint = Coordinate(x = 1, y = 0)
      val result = calculateSizeOfBasin(matrix, lowPoint)
      assert(result == 3)
    }

    "must return 9 for top-right basin in example input" in {
      val lowPoint = Coordinate(x = 9, y = 0)
      val result = calculateSizeOfBasin(matrix, lowPoint)
      assert(result == 9)
    }

    "must return 14 for middle basin in example input" in {
      val lowPoint = Coordinate(x = 2, y = 2)
      val result = calculateSizeOfBasin(matrix, lowPoint)
      assert(result == 14)
    }

    "must return 9 for bottom-right basin in example input" in {
      val lowPoint = Coordinate(x = 6, y = 4)
      val result = calculateSizeOfBasin(matrix, lowPoint)
      assert(result == 9)
    }
  }

  "part2" - {
    "must return 1134" - {
      "when given example input" in {
        val result = SmokeBasin.part2(file)
        assert(result == 1134)
      }
    }
  }
}
