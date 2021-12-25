package days.day25

import utils.FileReader.fileReader

import scala.annotation.tailrec

object SeaCucumber {

  def run(): Unit = {
    val file = "/day-25-input.txt"
    println(s"Day 25 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Long = {
    @tailrec
    def rec(matrix: Matrix, count: Long = 1): Long = {
      val newMatrix = step(matrix)
      if (areMatricesEqual(matrix, newMatrix)) {
        count
      } else {
        rec(newMatrix, count + 1)
      }
    }
    rec(initialiseMatrix(file))
  }

  private def areMatricesEqual(matrix1: Matrix, matrix2: Matrix): Boolean =
    matrix1.indices.forall(i => matrix1(i) sameElements matrix2(i))

  type Matrix = Array[Array[Char]]

  def step(beforeMoves: Matrix): Matrix = {
    val afterEastMove: Matrix = Array.ofDim(beforeMoves.length, beforeMoves.head.length)
    for (y <- beforeMoves.indices; x <- beforeMoves(y).indices) {
      val prevX = (if (x == 0) beforeMoves(y).length else x) - 1
      val nextX = if (x == beforeMoves(y).length - 1) 0 else x + 1
      (beforeMoves(y)(prevX), beforeMoves(y)(x), beforeMoves(y)(nextX)) match {
        case ('>', '.', _) | (_, '>', '>') | (_, '>', 'v') => afterEastMove(y)(x) = '>'
        case (_, 'v', _) => afterEastMove(y)(x) = 'v'
        case _ => afterEastMove(y)(x) = '.'
      }
    }

    val afterSouthMove: Matrix = Array.ofDim(afterEastMove.length, afterEastMove.head.length)
    for (y <- afterEastMove.indices; x <- afterEastMove(y).indices) {
      val prevY = (if (y == 0) afterEastMove.length else y) - 1
      val nextY = if (y == afterEastMove.length - 1) 0 else y + 1
      (afterEastMove(prevY)(x), afterEastMove(y)(x), afterEastMove(nextY)(x)) match {
        case ('v', '.', _) | (_, 'v', '>') | (_, 'v', 'v') => afterSouthMove(y)(x) = 'v'
        case (_, '>', _) => afterSouthMove(y)(x) = '>'
        case _ => afterSouthMove(y)(x) = '.'
      }
    }

    afterSouthMove
  }

  def initialiseMatrix(file: String): Matrix = fileReader(file).map(_.toArray).toArray

}
