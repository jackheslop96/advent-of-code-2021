package days.day21

import days.day21.DiracDice._
import org.scalatest.freespec.AnyFreeSpec

class DiracDiceSpec extends AnyFreeSpec {

  private val file = "/day-21-test-input.txt"

  "part1" - {
    "when given example input" - {
      "must return 739785" in {
        val result = part1(file)
        assert(result == 739785)
      }
    }
  }

  "Player" - {
    "takeTurn" - {
      "must return updated player and dice" - {

        "when player starts with 4 and rolls 1, 2, 3" in {
          val player = Player(4, 0)

          val result = player.takeTurn(Seq(1, 2, 3))

          assertResult(Player(10, 10))(result)
        }

        "when player starts with 8 and rolls 4,5,6" in {
          val player = Player(8, 0)

          val result = player.takeTurn(Seq(4, 5, 6))

          assertResult(Player(3, 3))(result)
        }
      }
    }
  }

  "part2" - {
    "when given example input" - {
      "must return 444356092776315" in {
        val result = part2(file)
        assert(result == 444356092776315L)
      }
    }
  }
}
