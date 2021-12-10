package days.day04

import utils.FileReader.fileReader

import scala.annotation.tailrec

case class Bingo(numbersToDraw: Seq[Int], boards: Seq[BingoBoard]) {
  def drawNumber: Bingo = this.copy(
    numbersToDraw = numbersToDraw.tail,
    boards = this.boards.map(_.updateBoard(numbersToDraw.head))
  )
}

object Bingo {

  def run(): Unit = {
    val result = run("/day-04-input.txt")
    println(s"Day 4 part 1 result: ${result.head}")
    println(s"Day 4 part 2 result: ${result.last}")
    println()
  }

  def run(file: String): Seq[Int] = {
    val input = fileReader(file)
    playGame(input)
  }

  def initialise(lines: Seq[String]): Bingo = {
    val drawnNumbers = lines.head.split(",").map(_.toInt).toSeq

    @tailrec
    def rec(lines: Seq[String], acc: Seq[BingoBoard] = Nil): Seq[BingoBoard] = {
      lines match {
        case Nil => acc
        case _ :: tail => rec(tail.drop(5), acc :+ BingoBoard.initialise(tail.take(5))) // 5x5 grid
      }
    }

    Bingo(drawnNumbers, rec(lines.tail))
  }

  def playGame(lines: Seq[String]): Seq[Int] = {
    @tailrec
    def rec(game: Bingo, acc: Seq[Int] = Nil): Seq[Int] = {
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
                acc = acc ++ boards.map(_.sumOfUnmarkedNumbers * number)
              )
          }
      }
    }

    rec(initialise(lines))
  }
}
