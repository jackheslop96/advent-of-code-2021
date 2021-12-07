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
    val positions = fileReader(file).head.split(",").map(_.toInt)
    val range = positions.min to positions.max

    /**
     *
     * @param aps all of the possible alignment positions for a given input
     * @param outcomes stores the total fuel usage for each alignment position
     * @return the minimum possible outcome
     */
    @tailrec
    def rec(aps: Seq[Int], outcomes: Seq[Int] = Nil): Int = {
      aps match {
        case Nil => outcomes.min
        case ap :: tail => rec(
          tail,
          outcomes :+ positions.map(fuelUsage(_, ap, burnsFuelAtConstantRate)).sum
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
