package bingo

import bingo.BingoBoard.initialise
import org.scalatest.freespec.AnyFreeSpec

class BingoBoardSpec extends AnyFreeSpec {

  private val input = Seq(
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19"
  )

  "initialise" - {
    "must initialise a bingo board" - {
      "when given a sequence of strings" in {
        val result = initialise(input)

        assert(result.numbers == Seq(
          Seq(BingoNumber(22), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
      }
    }
  }
  
  "updateBoard" - {
    "must update the board" - {
      "when given a number" in {
        val board = initialise(input)

        val result = board.updateBoard(22)

        assert(result.numbers == Seq(
          Seq(BingoNumber(22, hasBeenDrawn = true), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
      }
    }
  }
  
  "isComplete" - {
    "must return true" - {
      "when a row is complete" in {
        val board = BingoBoard(Seq(
          Seq(BingoNumber(22, true), BingoNumber(13, true), BingoNumber(17, true), BingoNumber(11, true), BingoNumber(0, true)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
        assertResult(true)(board.isComplete)
      }
      
      "when a column is complete" in {
        val board = BingoBoard(Seq(
          Seq(BingoNumber(22, true), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8, true), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21, true), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6, true), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1, true), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
        assertResult(true)(board.isComplete)
      }
    }
    
    "must return false" - {
      "when no rows or columns are complete" in {
        val board = BingoBoard(Seq(
          Seq(BingoNumber(22), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
        assertResult(false)(board.isComplete)
      }
    }
  }

  "sumOfUnmarkedNumbers" - {
    "when given the following input" - {
      "must return 300" in {
        val board = BingoBoard(Seq(
          Seq(BingoNumber(22), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))
        assertResult(300)(board.sumOfUnmarkedNumbers)
      }
    }
  }

}
