package days.day04

import days.day04.BingoNumber
import org.scalatest.freespec.AnyFreeSpec

class BingoNumberSpec extends AnyFreeSpec {

  "update" - {

    "must set hasBeenDrawn to true" - {
      "when number is drawn" in {
        val bingoNumber = BingoNumber(5)
        val result = bingoNumber.update(5)
        assertResult(BingoNumber(5, hasBeenDrawn = true))(result)
      }

      "when number has already been drawn" in {
        val bingoNumber = BingoNumber(5, hasBeenDrawn = true)
        val result = bingoNumber.update(6)
        assertResult(BingoNumber(5, hasBeenDrawn = true))(result)
      }
    }

    "must set hasBeenDrawn to false" - {
      "when number is not drawn and has not already been drawn" in {
        val bingoNumber = BingoNumber(5)
        val result = bingoNumber.update(6)
        assertResult(bingoNumber)(result)
      }
    }
  }
}
