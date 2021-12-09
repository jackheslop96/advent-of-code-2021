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
    findLowPoints(matrix)
      .map(_.value + 1)
      .sum
  }

  def part2(file: String): Int = {
    val matrix = initialiseMatrix(file)
    findLowPoints(matrix)
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

  private def findLowPoints(matrix: Matrix): Seq[Location] = {
    var basins: Seq[Location] = Nil
    for (x <- matrix.head.indices; y <- matrix.indices) {
      val value = matrix(y)(x)
      if (isLessThanAdjacents(matrix, value, x, y)) basins = basins :+ Location(Coordinate(x, y), value)
    }
    basins
  }

  private val directions = Seq(
    Direction(0, -1), // up
    Direction(1, 0),  // right
    Direction(0, 1),  // down
    Direction(-1, 0)  // left
  )

  private def isLessThanAdjacents(matrix: Matrix, value: Int, x: Int, y: Int): Boolean = {
    def isLessThanAdjacent(d: Direction): Boolean =
      try {
        value < matrix(y + d.y)(x + d.x)
      } catch {
        case _: ArrayIndexOutOfBoundsException => true
      }

    directions.forall(isLessThanAdjacent)
  }

  def calculateSizeOfBasin(matrix: Matrix, lowPoint: Coordinate): Int = {
    def rec(ds: Seq[Direction], coordinate: Coordinate, count: Int, checkedCoordinates: Seq[Coordinate]): (Int, Seq[Coordinate]) = {
      ds match {
        case Nil => (count, checkedCoordinates :+ coordinate)
        case head :: tail =>
          try {
            val newCoordinate = Coordinate(coordinate.x + head.x, coordinate.y + head.y)
            if (matrix(newCoordinate.y)(newCoordinate.x) < 9 && !checkedCoordinates.contains(newCoordinate)) {
              val (updatedCount, updatedCheckedCoordinates) = rec(directions, newCoordinate, count + 1, checkedCoordinates :+ coordinate)
              rec(tail, coordinate, updatedCount, updatedCheckedCoordinates)
            } else {
              rec(tail, coordinate, count, checkedCoordinates)
            }
          } catch {
            case _: ArrayIndexOutOfBoundsException => rec(tail, coordinate, count, checkedCoordinates)
          }
      }
    }

    // already know that the low point is part of the basin
    rec(directions, lowPoint, 1, Seq(lowPoint))._1
  }

}
