package days.day25

import days.day25.SeaCucumber._
import org.scalatest.freespec.AnyFreeSpec

class SeaCucumberSpec extends AnyFreeSpec {

  private val file = "/day-25-test-input.txt"

  "initialiseMatrix" - {
    "must initialise matrix" - {
      "when given example input" in {
        val result = initialiseMatrix(file)
        val expectedResult: Matrix = Array(
          Array('v','.','.','.','>','>','.','v','v','>'),
          Array('.','v','v','>','>','.','v','v','.','.'),
          Array('>','>','.','>','v','>','.','.','.','v'),
          Array('>','>','v','>','>','.','>','.','v','.'),
          Array('v','>','v','.','v','v','.','v','.','.'),
          Array('>','.','>','>','.','.','v','.','.','.'),
          Array('.','v','v','.','.','>','.','>','v','.'),
          Array('v','.','v','.','.','>','>','v','.','v'),
          Array('.','.','.','.','v','.','.','v','.','>')
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "step" - {
    "when a line of '>' moving sea cucumbers" - {
      "must only move right-most sea cucumber" in {
        val input: Matrix = Array(
          Array('.', '.', '.', '>', '>', '>', '>', '>', '.', '.', '.')
        )
        val result = step(input)
        val expectedResult: Matrix = Array(
          Array('.', '.', '.', '>', '>', '>', '>', '.', '>', '.', '.')
        )
        assertResult(expectedResult)(result)
      }

      "must loop back to start" in {
        val input: Matrix = Array(
          Array('.', '.', '.', '>', '>', '>', '>', '>')
        )
        val result = step(input)
        val expectedResult: Matrix = Array(
          Array('>', '.', '.', '>', '>', '>', '>', '.')
        )
        assertResult(expectedResult)(result)
      }

      "must not loop back to start if blocked" in {
        val input: Matrix = Array(
          Array('>','.','>')
        )
        val result = step(input)
        val expectedResult: Matrix = Array(
          Array('.','>','>')
        )
        assertResult(expectedResult)(result)
      }
    }

    "when south moving sea cucumbers" - {
      "must not loop back to start if space is taken" in {
        val input: Matrix = Array(
          Array('v'),
          Array('.'),
          Array('v')
        )
        val result = step(input)
        val expectedResult: Matrix = Array(
          Array('.'),
          Array('v'),
          Array('v')
        )
        assertResult(expectedResult)(result)
      }
    }

    "when a matrix of '>' and 'v' moving sea cucumbers" - {
      "must move '>' first then 'v'" in {
        val input: Matrix = Array(
          Array('.','.','.','.','.','.','.','.','.','.'),
          Array('.','>','v','.','.','.','.','v','.','.'),
          Array('.','.','.','.','.','.','.','>','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.')
        )
        val result = step(input)
        val expectedResult: Matrix = Array(
          Array('.','.','.','.','.','.','.','.','.','.'),
          Array('.','>','.','.','.','.','.','.','.','.'),
          Array('.','.','v','.','.','.','.','v','>','.'),
          Array('.','.','.','.','.','.','.','.','.','.')
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "part1" - {
    "when given example input" - {
      "must return 58" in {
        val result = part1(file)
        assert(result == 58)
      }
    }
  }

}
