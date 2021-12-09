package days.day9

import utils.FileReader.fileReader

object SmokeBasin {

  def run(): Unit = {
    val file = "/day-9-input.txt"
    println(s"Day 9 part 1 result: ${run(file)}")
    println()
  }

  def run(file: String): Int = {
    val matrix = initialiseMatrix(file)
    var counter = 0
    for (x <- matrix.head.indices; y <- matrix.indices) {
      val value = matrix(y)(x)
      if (isLessThanAdjacents(matrix, value, x, y)) counter = counter + value + 1
    }
    counter
  }

  def initialiseMatrix(file: String): Array[Array[Int]] = {
    fileReader(file)
      .map(_.map(_.toString.toInt).toArray)
      .toArray
  }

  private def isLessThanAdjacents(matrix: Array[Array[Int]], value: Int, x: Int, y: Int): Boolean = {
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

}
