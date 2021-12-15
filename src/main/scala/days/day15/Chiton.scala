package days.day15

import utils.FileReader.fileReader

object Chiton {

  type Matrix = Array[Array[Int]]

  def run(): Unit = {
    val file = "/day-15-input.txt"
    println(s"Day 15 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Int = {
    val matrix = initialiseMatrix(file)
    findPaths(matrix).min
  }

  private def findPaths(matrix: Matrix): Seq[Int] = {

    val maxY = matrix.length - 1

    def rec(x: Int = 0, y: Int = 0, path: Int = -matrix(0)(0)): Seq[Int] = {
      val maxX = matrix(y).length - 1
      val updatedPath = path + matrix(y)(x)
      (x, y) match {
        case _ if x < maxX && y < maxY =>
          rec(x + 1, y, updatedPath) ++ rec(x, y + 1, updatedPath)
        case _ if x < maxX =>
          rec(x + 1, y, updatedPath)
        case _ if y < maxY =>
          rec(x, y + 1, updatedPath)
        case _ =>
          Seq(updatedPath)
      }
    }

    rec()
  }

  private def initialiseMatrix(file: String): Matrix =
    fileReader(file)
      .map(_.map(_.toString.toInt).toArray)
      .toArray

}
