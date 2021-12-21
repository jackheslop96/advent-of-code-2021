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
          val dice = Dice(0)
          val player = Player(1, 4, 0, 0)

          val result = player.takeTurn(dice)

          assertResult(Player(1, 10, 10, 1))(result._1)
          assertResult(Dice(3))(result._2)
        }

        "when player starts with 8 and rolls 4,5,6" in {
          val dice = Dice(3)
          val player = Player(2, 8, 0, 0)

          val result = player.takeTurn(dice)

          assertResult(Player(2, 3, 3, 1))(result._1)
          assertResult(Dice(6))(result._2)
        }
      }
    }
  }
}
