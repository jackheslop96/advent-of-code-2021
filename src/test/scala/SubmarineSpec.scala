import Submarine._
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

  ".applyInstructions" - {

    "must return horizontal position of 15 and depth of 60 for 'forward 5, down 5, forward 8, up 3, down 8, forward 2'" in {
      val instructions = Seq("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
      val result = applyInstructions(instructions)
      assert(result.horizontalPosition == 15)
      assert(result.depth == 60)
      assert(result.multiplyPositions == 900)
    }
  }

  "multiplyPositions" - {
    val result = Submarine(15, 10, 0)
    assert(result.multiplyPositions == 150)
  }
}
