package day4

import days.day4.Bingo._
import days.day4.{BingoBoard, BingoNumber}
import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader.fileReader

class BingoSpec extends AnyFreeSpec {

  private val numbersToDraw = "22,4,9,5,11"

  private val board = Seq(
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19"
  )

  private val input = numbersToDraw :: "" :: Nil ++ board

  "initialise" - {
    "must initialise a game of bingo" - {
      "when given a sequence of strings" in {
        val result = initialise(input)

        assert(result.numbersToDraw == Seq(22, 4, 9, 5, 11))
        assert(result.boards == Seq(BingoBoard(Seq(
          Seq(BingoNumber(22), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))))
      }
    }
  }

  "drawNumber" - {
    "must update the game" - {
      "when drawing a number" in {
        val game = initialise(input)
        val result = game.drawNumber

        assert(result.numbersToDraw == Seq(4, 9, 5, 11))
        assert(result.boards == Seq(BingoBoard(Seq(
          Seq(BingoNumber(22, hasBeenDrawn = true), BingoNumber(13), BingoNumber(17), BingoNumber(11), BingoNumber(0)),
          Seq(BingoNumber(8), BingoNumber(2), BingoNumber(23), BingoNumber(4), BingoNumber(24)),
          Seq(BingoNumber(21), BingoNumber(9), BingoNumber(14), BingoNumber(16), BingoNumber(7)),
          Seq(BingoNumber(6), BingoNumber(10), BingoNumber(3), BingoNumber(18), BingoNumber(5)),
          Seq(BingoNumber(1), BingoNumber(12), BingoNumber(20), BingoNumber(15), BingoNumber(19))
        ))))
      }
    }
  }

  "playGame" - {
    "when given example input" - {
      val lines = fileReader("/day-4-test-input.txt")
      val result = playGame(lines)
      "must return score of 4512 for winner" in {
        assert(result.head == 4512)
      }
      "must return score of 1924 for loser" in {
        assert(result.last == 1924)
      }
    }
  }

}
