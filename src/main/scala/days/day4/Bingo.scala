package days.day4

import utils.FileReader.stringFileReader

import scala.annotation.tailrec

case class Bingo(numbersToDraw: Seq[Int], boards: Seq[BingoBoard]) {
  def drawNumber: Bingo = this.copy(
    numbersToDraw = numbersToDraw.tail,
    boards = this.boards.map(_.updateBoard(numbersToDraw.head))
  )
}

object Bingo {

  def run(): Unit = {
    val input = stringFileReader("/day-4-input.txt")
    println("Day 4 result:")
    playGame(input).foreach(x => println(s"${x._1}: ${x._2}"))
    println()
  }

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

  def playGame(lines: Seq[String]): Seq[(Int, Int)] = {
    @tailrec
    def rec(game: Bingo, acc: Seq[(Int, Int)] = Nil): Seq[(Int, Int)] = {
      game.numbersToDraw match {
        case Nil =>
          acc
        case numbers =>
          val number = numbers.head
          val updatedGame = game.copy(
            numbersToDraw = numbers.tail,
            boards = game.boards.map(_.updateBoard(number))
          )
          val updatedBoards = updatedGame.boards
          updatedBoards.filter(_.isComplete) match {
            case Nil =>
              rec(updatedGame, acc)
            case boards =>
              rec(
                game = updatedGame.copy(boards = updatedBoards.filterNot(_.isComplete)),
                acc = acc ++ boards.map(x => (acc.size + 1, x.sumOfUnmarkedNumbers * number))
              )
          }
      }
    }

    rec(initialise(lines))
  }
}
