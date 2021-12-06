package days.day6

import utils.FileReader.stringFileReader

import scala.annotation.tailrec

object LanternFish {

  def run(): Unit = {
    val file = "/day-6-input.txt"
    println(s"Day 6 part 1 result: ${run(file, 80)}")
    println(s"Day 6 part 2 result: ${run(file, 256)}")
    println()
  }

  def run(file: String, numberOfDays: Int): Long = {
    val input = parseInput(stringFileReader(file))
    val map = simulateDays(getMap(input), numberOfDays)
    map.values.sum
  }

  def parseInput(input: Seq[String]): Seq[Int] = {
    input
      .head
      .split(",")
      .map(_.toInt)
  }

  def getMap(input: Seq[Int]): Map[Int, Long] = {
    input
      .groupBy(identity)
      .map(f => (f._1, f._2.length))
  }

  def simulateDays(map: Map[Int, Long], numberOfDays: Int): Map[Int, Long] = {
    @tailrec
    def rec(acc: Map[Int, Long], daysRemaining: Int): Map[Int, Long] = {
      daysRemaining match {
        case 0 => acc
        case _ => rec(simulateDay(acc), daysRemaining - 1)
      }
    }

    rec(map, numberOfDays)
  }

  def simulateDay(map: Map[Int, Long]): Map[Int, Long] = {
    def combineMaps(a: Map[Int, Long], b: Map[Int, Long]): Map[Int, Long] = {
      a ++ b.map {
        case (k, v) => k -> (v + a.getOrElse(k, 0L))
      }
    }

    combineMaps(
      map.map(f => f.copy(_1 = f._1 - 1)),
      map.get(0) match {
        case Some(count) => Map(6 -> count) ++ Map(8 -> count)
        case None => Map()
      }
    ).removed(-1)
  }
}
