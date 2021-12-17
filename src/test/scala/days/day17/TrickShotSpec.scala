package days.day17

import days.day17.TrickShot._
import org.scalatest.freespec.AnyFreeSpec

class TrickShotSpec extends AnyFreeSpec {

  private val file = "/day-17-test-input.txt"

  "part1" - {
    "when example input" - {
      "must return 45" in {
        val result = part1(file)
        assert(result == 45)
      }
    }
  }

  "part2" - {
    "when example input" - {
      "must return 112" in {
        val result = part2(file)
        assert(result == 112)
      }
    }
  }
}
