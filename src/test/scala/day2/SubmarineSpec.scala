package day2

import days.day2.Submarine
import org.scalatest.freespec.AnyFreeSpec

class SubmarineSpec extends AnyFreeSpec {

  "apply" - {
    "must initialise new submarine with horizontal position 0 and depth 0 and aim 0" in {
      val result = Submarine()
      assert(result.horizontalPosition == 0)
      assert(result.depth == 0)
      assert(result.aim == 0)
    }
  }

  "applyInstruction" - {

    "must return original positions for unrecognised instruction" in {
      val submarine = Submarine()
      val result = submarine.applyInstruction("foo")
      assert(result.horizontalPosition == 0)
      assert(result.depth == 0)
    }

    "must add 1 to aim for instruction 'down 1'" in {
      val submarine = Submarine()
      val result = submarine.applyInstruction("down 1")
      assert(result.aim == 1)
    }

    "must subtract 1 from aim for instruction 'up 1'" in {
      val submarine = Submarine()
      val result = submarine.applyInstruction("up 1")
      assert(result.aim == -1)
    }

    "must increase depth by the aim for instruction 'forward 1'" in {
      val aim = 5
      val submarine = Submarine(0, 0, aim)
      val result = submarine.applyInstruction("forward 1")
      assert(result.depth == aim)
    }

    "must increase depth by twice the aim for instruction 'forward 2'" in {
      val aim = 5
      val submarine = Submarine(0, 0, aim)
      val result = submarine.applyInstruction("forward 2")
      assert(result.depth == 2 * aim)
    }
  }

  "multiplyPositions" - {
    "must return product of horizontal position and depth" in {
      val result = Submarine(15, 10, 0)
      assert(result.multiplyPositions == 150)
    }
  }

  "run" - {
    "when given example input" - {
      "must return 900" in {
        val result = Submarine.run("/day-2-test-input.txt")
        assert(result == 900)
      }
    }
  }
}
