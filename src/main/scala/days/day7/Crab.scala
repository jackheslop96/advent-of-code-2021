package days.day7

import utils.FileReader.fileReader

import scala.annotation.tailrec

object Crab {

  def run(): Unit = {
    val file = "/day-7-input.txt"
    println(s"Day 7 part 1 result: ${run(file, burnsFuelAtConstantRate = true)}")
    println(s"Day 7 part 2 result: ${run(file, burnsFuelAtConstantRate = false)}")
    println()
  }

  def run(file: String, burnsFuelAtConstantRate: Boolean): Int = {
    val input = fileReader(file).head.split(",").map(_.toInt)
    val range = input.min to input.max

    @tailrec
    def rec(xs: Seq[Int], acc: Seq[Int] = Nil): Int = {
      xs match {
        case Nil => acc.min
        case head :: tail => rec(
          tail,
          acc :+ input.map(x => fuelUsage(x, head, burnsFuelAtConstantRate)).sum
        )
      }
    }

    rec(range.toList)
  }

  private def fuelUsage(startPoint: Int, endPoint: Int, burnsFuelAtConstantRate: Boolean): Int = {
    val distance = Math.abs(endPoint - startPoint)
    if (burnsFuelAtConstantRate) distance else (1 to distance).sum
  }
}
