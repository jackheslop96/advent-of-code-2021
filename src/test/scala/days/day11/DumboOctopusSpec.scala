package days.day11

import days.day11.DumboOctopus._
import org.scalatest.freespec.AnyFreeSpec

class DumboOctopusSpec extends AnyFreeSpec {

  private val file = "/day-11-test-input.txt"

  "step" - {

    "when small matrix" - {

      "when given initial matrix" - {
        "must return matrix after step 1" in {
          val input: Matrix = Array(
            Array(1, 1, 1, 1, 1),
            Array(1, 9, 9, 9, 1),
            Array(1, 9, 1, 9, 1),
            Array(1, 9, 9, 9, 1),
            Array(1, 1, 1, 1, 1)
          ).map(_.map(DumboOctopus(_)))

          val expectedResult: Matrix = Array(
            Array(3, 4, 5, 4, 3),
            Array(4, 0, 0, 0, 4),
            Array(5, 0, 0, 0, 5),
            Array(4, 0, 0, 0, 4),
            Array(3, 4, 5, 4, 3)
          ).map(_.map(DumboOctopus(_)))

          val result = step(input)
          assertResult(expectedResult)(result.matrix)
          assert(result.numberOfFlashes == 9)
        }
      }

      "when given step 1 matrix" - {
        "must return matrix after step 2" in {
          val input: Matrix = Array(
            Array(3, 4, 5, 4, 3),
            Array(4, 0, 0, 0, 4),
            Array(5, 0, 0, 0, 5),
            Array(4, 0, 0, 0, 4),
            Array(3, 4, 5, 4, 3)
          ).map(_.map(DumboOctopus(_)))

          val expectedResult: Matrix = Array(
            Array(4, 5, 6, 5, 4),
            Array(5, 1, 1, 1, 5),
            Array(6, 1, 1, 1, 6),
            Array(5, 1, 1, 1, 5),
            Array(4, 5, 6, 5, 4)
          ).map(_.map(DumboOctopus(_)))

          val result = step(input)
          assertResult(expectedResult)(result.matrix)
          assert(result.numberOfFlashes == 0)
        }
      }
    }

    "when large matrix" - {

      "when given initial matrix" - {
        "must return matrix after step 1" in {
          val input: Matrix = Array(
            Array(5,4,8,3,1,4,3,2,2,3),
            Array(2,7,4,5,8,5,4,7,1,1),
            Array(5,2,6,4,5,5,6,1,7,3),
            Array(6,1,4,1,3,3,6,1,4,6),
            Array(6,3,5,7,3,8,5,4,7,8),
            Array(4,1,6,7,5,2,4,6,4,5),
            Array(2,1,7,6,8,4,1,7,2,1),
            Array(6,8,8,2,8,8,1,1,3,4),
            Array(4,8,4,6,8,4,8,5,5,4),
            Array(5,2,8,3,7,5,1,5,2,6)
          ).map(_.map(DumboOctopus(_)))

          val expectedResult: Matrix = Array(
            Array(6,5,9,4,2,5,4,3,3,4),
            Array(3,8,5,6,9,6,5,8,2,2),
            Array(6,3,7,5,6,6,7,2,8,4),
            Array(7,2,5,2,4,4,7,2,5,7),
            Array(7,4,6,8,4,9,6,5,8,9),
            Array(5,2,7,8,6,3,5,7,5,6),
            Array(3,2,8,7,9,5,2,8,3,2),
            Array(7,9,9,3,9,9,2,2,4,5),
            Array(5,9,5,7,9,5,9,6,6,5),
            Array(6,3,9,4,8,6,2,6,3,7)
          ).map(_.map(DumboOctopus(_)))

          val result = step(input).matrix
          assertResult(expectedResult)(result)
        }
      }

      "when given step 1 matrix" - {
        "must return matrix after step 2" in {
          val input: Matrix = Array(
            Array(6,5,9,4,2,5,4,3,3,4),
            Array(3,8,5,6,9,6,5,8,2,2),
            Array(6,3,7,5,6,6,7,2,8,4),
            Array(7,2,5,2,4,4,7,2,5,7),
            Array(7,4,6,8,4,9,6,5,8,9),
            Array(5,2,7,8,6,3,5,7,5,6),
            Array(3,2,8,7,9,5,2,8,3,2),
            Array(7,9,9,3,9,9,2,2,4,5),
            Array(5,9,5,7,9,5,9,6,6,5),
            Array(6,3,9,4,8,6,2,6,3,7)
          ).map(_.map(DumboOctopus(_)))

          val expectedResult: Matrix = Array(
            Array(8,8,0,7,4,7,6,5,5,5),
            Array(5,0,8,9,0,8,7,0,5,4),
            Array(8,5,9,7,8,8,9,6,0,8),
            Array(8,4,8,5,7,6,9,6,0,0),
            Array(8,7,0,0,9,0,8,8,0,0),
            Array(6,6,0,0,0,8,8,9,8,9),
            Array(6,8,0,0,0,0,5,9,4,3),
            Array(0,0,0,0,0,0,7,4,5,6),
            Array(9,0,0,0,0,0,0,8,7,6),
            Array(8,7,0,0,0,0,6,8,4,8)
          ).map(_.map(DumboOctopus(_)))

          val result = step(input).matrix
          assertResult(expectedResult)(result)
        }
      }
    }
  }

  "part1" - {
    "must return 1656 flashes" - {
      "when simulating 100 steps of the example input" in {
        val result = part1(file, 100)
        assert(result == 1656)
      }
    }
  }
}
