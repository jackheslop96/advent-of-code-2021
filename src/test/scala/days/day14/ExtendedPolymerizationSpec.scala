package days.day14

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

}
