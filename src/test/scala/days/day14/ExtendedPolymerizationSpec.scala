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

  "individualCharacterCount" - {
    "must return count of each character" - {

      "when one character the same" in {
        // ABCD
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BC" -> 1, "CD" -> 1)
        val result = individualCharacterCount(input, "ABCD")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 1, 'C' -> 1, 'D' -> 1)
        assertResult(expectedResult)(result)
      }

      "when one character duplicated" in {
        // ABBC
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BB" -> 1, "BC" -> 1)
        val result = individualCharacterCount(input, "ABBC")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 2, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

      "when start character duplicated" in {
        // BABBC
        val input: Map[String, BigDecimal] = Map("BA" -> 1, "AB" -> 1, "BB" -> 1, "BC" -> 1)
        val result = individualCharacterCount(input, "BABBC")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 3, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

      "when end character duplicated" in {
        // ABBCB
        val input: Map[String, BigDecimal] = Map("AB" -> 1, "BB" -> 1, "BC" -> 1, "CB" -> 1)
        val result = individualCharacterCount(input, "ABBCB")
        val expectedResult: Map[Char, BigDecimal] = Map('A' -> 1, 'B' -> 3, 'C' -> 1)
        assertResult(expectedResult)(result)
      }

    }
  }

}
