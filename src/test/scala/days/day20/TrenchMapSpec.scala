package days.day20

import days.day20.TrenchMap._
import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader.fileReader

class TrenchMapSpec extends AnyFreeSpec {

  private val file = "/day-20-test-input.txt"

  "run" - {
    "must return 35" - {
      "when algorithm applied twice on example input" in {
        val result = TrenchMap.run(file, 2)
        assert(result == 35)
      }
    }

    "must return 3351" - {
      "when algorithm applied twice on example input" in {
        val result = TrenchMap.run(file, 50)
        assert(result == 3351)
      }
    }
  }

  "expandImage" - {
    "must surround with enough dots to represent infinite grid" - {

      "when given example image" in {
        val input = Array(
          Array('#','.','.','#','.'),
          Array('#','.','.','.','.'),
          Array('#','#','.','.','#'),
          Array('.','.','#','.','.'),
          Array('.','.','#','#','#')
        )

        val expectedResult = Array(
          Array('.','.','.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','#','.','.','#','.','.','.','.'),
          Array('.','.','.','#','.','.','.','.','.','.','.'),
          Array('.','.','.','#','#','.','.','#','.','.','.'),
          Array('.','.','.','.','.','#','.','.','.','.','.'),
          Array('.','.','.','.','.','#','#','#','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.','.','.')
        )

        val result = expandImage(input)

        assertResult(expectedResult)(result)
      }

      "when given all hashtags" in {
        val input = Array(
          Array('#','#','#'),
          Array('#','#','#'),
          Array('#','#','#')
        )

        val expectedResult = Array(
          Array('.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','#','#','#','.','.','.'),
          Array('.','.','.','#','#','#','.','.','.'),
          Array('.','.','.','#','#','#','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.'),
          Array('.','.','.','.','.','.','.','.','.')
        )

        val result = expandImage(input)

        assertResult(expectedResult)(result)
      }

      "when default char is hashtag" in {
        val input = Array(
          Array('.','.','.'),
          Array('.','.','.'),
          Array('.','.','.')
        )

        val expectedResult = Array(
          Array('#','#','#','#','#','#','#','#','#'),
          Array('#','#','#','#','#','#','#','#','#'),
          Array('#','#','#','#','#','#','#','#','#'),
          Array('#','#','#','.','.','.','#','#','#'),
          Array('#','#','#','.','.','.','#','#','#'),
          Array('#','#','#','.','.','.','#','#','#'),
          Array('#','#','#','#','#','#','#','#','#'),
          Array('#','#','#','#','#','#','#','#','#'),
          Array('#','#','#','#','#','#','#','#','#')
        )

        val result = expandImage(input, '#')

        assertResult(expectedResult)(result)
      }
    }
  }

  "applyAlgorithm" - {
    "when step 1" in {
      val input = Array(
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','#','.','.','#','.','.','.','.'),
        Array('.','.','.','#','.','.','.','.','.','.','.'),
        Array('.','.','.','#','#','.','.','#','.','.','.'),
        Array('.','.','.','.','.','#','.','.','.','.','.'),
        Array('.','.','.','.','.','#','#','#','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.')
      )
      
      val expectedResult = Array(
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','#','#','.','#','#','.','.','.'),
        Array('.','.','#','.','.','#','.','#','.','.','.'),
        Array('.','.','#','#','.','#','.','.','#','.','.'),
        Array('.','.','#','#','#','#','.','.','#','.','.'),
        Array('.','.','.','#','.','.','#','#','.','.','.'),
        Array('.','.','.','.','#','#','.','.','#','.','.'),
        Array('.','.','.','.','.','#','.','#','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.'),
        Array('.','.','.','.','.','.','.','.','.','.','.')
      )

      val algorithm = fileReader(file).head
      
      val result = applyAlgorithm(TrenchFloor(algorithm, input), '.')

      assertResult(expectedResult)(result.image)
    }
  }

}
