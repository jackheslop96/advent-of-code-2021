package days.day9

import utils.FileReader.fileReader

object SmokeBasin {

  type Matrix = Array[Array[Int]]

  case class Coordinate(x: Int, y: Int)
  case class Direction(x: Int, y: Int)
  case class Location(coordinate: Coordinate, value: Int)

  def run(): Unit = {
    val file = "/day-9-input.txt"
    println(s"Day 9 part 1 result: ${part1(file)}")
    println(s"Day 9 part 2 result: ${part2(file)}")
    println()
  }

  def part1(file: String): Int = {
    val matrix = initialiseMatrix(file)
    findBasins(matrix).map(_.value + 1).sum
  }

  def part2(file: String): Int = {
    val matrix = initialiseMatrix(file)
    findBasins(matrix)
      .map(lowPoint => calculateSizeOfBasin(matrix, lowPoint.coordinate))
      .sortWith(_ > _)
      .take(3)
      .product
  }

  def initialiseMatrix(file: String): Matrix = {
    fileReader(file)
      .map(_.map(_.toString.toInt).toArray)
      .toArray
  }

  private def findBasins(matrix: Matrix): Seq[Location] = {
    var basins: Seq[Location] = Nil
    for (x <- matrix.head.indices; y <- matrix.indices) {
      val value = matrix(y)(x)
      if (isLessThanAdjacents(matrix, value, x, y)) basins = basins :+ Location(Coordinate(x, y), value)
    }
    basins
  }

  private def isLessThanAdjacents(matrix: Matrix, value: Int, x: Int, y: Int): Boolean = {
    def isLessThanAdjacent(x: Int, y: Int): Boolean =
      try {
        value < matrix(y)(x)
      } catch {
        case _: ArrayIndexOutOfBoundsException => true
      }

    isLessThanAdjacent(x, y - 1) &&
      isLessThanAdjacent(x + 1, y) &&
      isLessThanAdjacent(x, y + 1) &&
      isLessThanAdjacent(x - 1, y)
  }

  def calculateSizeOfBasin(matrix: Matrix, lowPoint: Coordinate): Int = {
    val directions = Seq(
      Direction(0, -1), // up
      Direction(1, 0),  // right
      Direction(0, 1),  // down
      Direction(-1, 0)  // left
    )

    def rec(ds: Seq[Direction], coordinate: Coordinate, checkedCoordinates: Seq[Coordinate], acc: Int): (Int, Seq[Coordinate]) = {
      ds match {
        case Nil => (acc, checkedCoordinates :+ coordinate)
        case head :: tail =>
          try {
            val newCoordinate = Coordinate(coordinate.x + head.x, coordinate.y + head.y)
            if (matrix(newCoordinate.y)(newCoordinate.x) < 9 && !checkedCoordinates.contains(newCoordinate)) {
              val res = rec(directions, newCoordinate, checkedCoordinates :+ coordinate, acc + 1)
              rec(tail, coordinate, res._2, res._1)
            } else {
              rec(tail, coordinate, checkedCoordinates, acc)
            }
          } catch {
            case _: ArrayIndexOutOfBoundsException => rec(tail, coordinate, checkedCoordinates, acc)
          }
      }
    }

    // already know that the low point is part of the basin
    rec(directions, lowPoint, Seq(lowPoint), 1)._1
  }

}
