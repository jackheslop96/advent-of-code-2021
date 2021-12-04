package bingo

import scala.annotation.tailrec

case class Bingo(numbersToDraw: Seq[Int], boards: Seq[BingoBoard]) {
  def drawNumber: Bingo = this.copy(
    numbersToDraw = numbersToDraw.tail,
    boards = this.boards.map(_.updateBoard(numbersToDraw.head))
  )
}

object Bingo {
  def initialise(lines: Seq[String]): Bingo = {
    val drawnNumbers = lines.head.split(",").map(_.toInt).toSeq

    @tailrec
    def rec(lines: Seq[String], acc: Seq[BingoBoard] = Nil): Seq[BingoBoard] = {
      lines match {
        case Nil => acc
        case _ :: tail => rec(tail.drop(5), acc :+ BingoBoard.initialise(tail.take(5)))
      }
    }

    Bingo(drawnNumbers, rec(lines.tail))
  }

  def playGame(lines: Seq[String]): Int = {
    @tailrec
    def rec(game: Bingo): Int = {
      game.numbersToDraw match {
        case Nil => 0
        case numbers =>
          val number = numbers.head
          val updatedGame = game.copy(
            numbersToDraw = numbers.tail,
            boards = game.boards.map(_.updateBoard(number))
          )
          updatedGame.boards.find(_.isComplete) match {
            case Some(board) => number * board.sumOfUnmarkedNumbers
            case None => rec(updatedGame)
          }
      }
    }

    rec(initialise(lines))
  }

  def loseOnPurpose(lines: Seq[String]): Int = {
    @tailrec
    def rec(game: Bingo, losingBoardIndex: Option[Int] = None): Int = {
      game.numbersToDraw match {
        case Nil => 0
        case numbers =>
          val number = numbers.head
          val updatedGame = game.copy(
            numbersToDraw = numbers.tail,
            boards = game.boards.map(_.updateBoard(number))
          )

          (updatedGame.boards.zipWithIndex.filter(!_._1.isComplete), losingBoardIndex) match {
            case (Nil, Some(index)) => number * updatedGame.boards(index).sumOfUnmarkedNumbers
            case ((_, index) :: Nil, _) => rec(updatedGame, Some(index))
            case _ => rec(updatedGame)
          }
      }
    }

    rec(initialise(lines))
  }
}
