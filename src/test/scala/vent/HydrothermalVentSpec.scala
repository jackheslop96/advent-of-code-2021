package vent

import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader.stringFileReader
import vent.HydrothermalVent._

class HydrothermalVentSpec extends AnyFreeSpec {

  "generateListOfCoordinates" - {
    "must generate a list of coordinates" - {
      "when given a coordinate pair with increasing x" in {
        val input = "0,9 -> 5,9"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(0, 9),
          Coordinate(1, 9),
          Coordinate(2, 9),
          Coordinate(3, 9),
          Coordinate(4, 9),
          Coordinate(5, 9)
        )
        assertResult(expectedResult)(result)
      }

      "when given a coordinate pair with decreasing x" in {
        val input = "5,9 -> 0,9"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(5, 9),
          Coordinate(4, 9),
          Coordinate(3, 9),
          Coordinate(2, 9),
          Coordinate(1, 9),
          Coordinate(0, 9)
        )
        assertResult(expectedResult)(result)
      }

      "when given a coordinate pair with increasing y" in {
        val input = "2,1 -> 2,2"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(2, 1),
          Coordinate(2, 2)
        )
        assertResult(expectedResult)(result)
      }

      "when given a coordinate pair with decreasing y" in {
        val input = "2,2 -> 2,1"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(2, 2),
          Coordinate(2, 1)
        )
        assertResult(expectedResult)(result)
      }

      "when given a coordinate pair with increasing x and y" in {
        val input = "0,0 -> 8,8"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(0, 0),
          Coordinate(1, 1),
          Coordinate(2, 2),
          Coordinate(3, 3),
          Coordinate(4, 4),
          Coordinate(5, 5),
          Coordinate(6, 6),
          Coordinate(7, 7),
          Coordinate(8, 8)
        )
        assertResult(expectedResult)(result)
      }

      "when given a coordinate pair with decreasing x and y" in {
        val input = "8,0 -> 0,8"
        val result = generateListOfCoordinates(input)
        val expectedResult = Seq(
          Coordinate(8, 0),
          Coordinate(7, 1),
          Coordinate(6, 2),
          Coordinate(5, 3),
          Coordinate(4, 4),
          Coordinate(3, 5),
          Coordinate(2, 6),
          Coordinate(1, 7),
          Coordinate(0, 8)
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "isLineHorizontalOrVertical" - {
    "must return true" - {
      "when line is horizontal" in {
        val result = isLineHorizontalOrVertical("0,9 -> 5,9")
        assertResult(result)(true)
      }

      "when line is vertical" in {
        val result = isLineHorizontalOrVertical("2,2 -> 2,1")
        assertResult(result)(true)
      }
    }

    "must return false" - {
      "when line is neither horizontal nor vertical" in {
        val result = isLineHorizontalOrVertical("8,0 -> 0,8")
        assertResult(result)(false)
      }
    }
  }

  "generateAllCoordinates" - {
    "must generate all coordinates" - {
      "when given a list of coordinate pair" in {
        val input = stringFileReader("/day-5-test-input.txt")
        val result = generateAllCoordinates(input)
        val expectedResult = Seq(
          Coordinate(0, 9),
          Coordinate(1, 9),
          Coordinate(2, 9),
          Coordinate(3, 9),
          Coordinate(4, 9),
          Coordinate(5, 9),

          Coordinate(8, 0),
          Coordinate(7, 1),
          Coordinate(6, 2),
          Coordinate(5, 3),
          Coordinate(4, 4),
          Coordinate(3, 5),
          Coordinate(2, 6),
          Coordinate(1, 7),
          Coordinate(0, 8),

          Coordinate(9, 4),
          Coordinate(8, 4),
          Coordinate(7, 4),
          Coordinate(6, 4),
          Coordinate(5, 4),
          Coordinate(4, 4),
          Coordinate(3, 4),

          Coordinate(2, 2),
          Coordinate(2, 1),

          Coordinate(7, 0),
          Coordinate(7, 1),
          Coordinate(7, 2),
          Coordinate(7, 3),
          Coordinate(7, 4),

          Coordinate(6, 4),
          Coordinate(5, 3),
          Coordinate(4, 2),
          Coordinate(3, 1),
          Coordinate(2, 0),

          Coordinate(0, 9),
          Coordinate(1, 9),
          Coordinate(2, 9),

          Coordinate(3, 4),
          Coordinate(2, 4),
          Coordinate(1, 4),

          Coordinate(0, 0),
          Coordinate(1, 1),
          Coordinate(2, 2),
          Coordinate(3, 3),
          Coordinate(4, 4),
          Coordinate(5, 5),
          Coordinate(6, 6),
          Coordinate(7, 7),
          Coordinate(8, 8),

          Coordinate(5, 5),
          Coordinate(6, 4),
          Coordinate(7, 3),
          Coordinate(8, 2)
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "countDangerousAreas" - {
    "when list is empty" - {
      "must return 0" in {
        val result = countDangerousAreas(Seq())
        assert(result == 0)
      }
    }

    "when there is 1 dangerous area" - {
      "must return 1" in {
        val result = countDangerousAreas(Seq(
          Coordinate(0, 0),
          Coordinate(0, 0)
        ))
        assert(result == 1)
      }
    }

    "when there are 2 dangerous area" - {
      "must return 2" in {
        val result = countDangerousAreas(Seq(
          Coordinate(0, 0),
          Coordinate(0, 0),
          Coordinate(0, 1),
          Coordinate(0, 2),
          Coordinate(0, 2),
          Coordinate(0, 2)
        ))
        assert(result == 2)
      }
    }
  }

}
