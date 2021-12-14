package days.day14

import days.day14.ExtendedPolymerization._
import org.scalatest.freespec.AnyFreeSpec

class ExtendedPolymerizationSpec extends AnyFreeSpec {

  private val file = "/day-14-test-input.txt"

  "run" - {
    "must return 1588 after 10 steps" - {
      "when given example input" in {
        val result = ExtendedPolymerization.run(file, 10)
        assert(result == 1588L)
      }
    }
    "must return 2188189693529 after 40 steps" - {
      "when given example input" in {
        val result = ExtendedPolymerization.run(file, 40)
        assert(result == 2188189693529L)
      }
    }
  }

  "individualCharacterCounts" - {
    "must return count of each character" - {

      "when ABCD" in {
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BC" -> 1, "CD" -> 1)
        val result = individualCharacterCounts(input, "ABCD")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 1, 'C' -> 1, 'D' -> 1)
        assertResult(expectedResult)(result)
      }

      "when ABBC" in {
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BB" -> 1, "BC" -> 1)
        val result = individualCharacterCounts(input, "ABBC")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 2, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

      "when BABBC" in {
        val input: Map[String, BigDecimal] = Map("BA" -> 1, "AB" -> 1, "BB" -> 1, "BC" -> 1)
        val result = individualCharacterCounts(input, "BABBC")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 3, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

      "when ABBCB" in {
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BB" -> 1, "BC" -> 1, "CB" -> 1)
        val result = individualCharacterCounts(input, "ABBCB")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 3, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

    }
  }

}
