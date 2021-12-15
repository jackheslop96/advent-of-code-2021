package days.day15

import days.day15.Chiton._
import org.scalatest.freespec.AnyFreeSpec

class ChitonSpec extends AnyFreeSpec {

  private val file = "/day-15-test-input.txt"

  "part1" - {
    "must return 40" - {
      "when given example input" in {
        val result = part1(file)
        assert(result == 40)
      }
    }
  }

}
