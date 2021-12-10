package days.day01

import utils.FileReader.fileReader

import scala.annotation.tailrec

object DepthIncreaseCounter {

  def run(): Unit = {
    val file = "/day-01-input.txt"
    println(s"Day 1 part 1 result: ${run(file, 1)}")
    println(s"Day 1 part 2 result: ${run(file, 3)}")
    println()
  }

  def run(file: String, windowSize: Int): Int = {
    val input = fileReader(file).map(_.toInt)
    countIncrements(input, windowSize)
  }

  def countIncrements(depths: Seq[Int], windowSize: Int): Int = {

    def sumOfFirstWindow(xs: Seq[Int]): Int = xs.take(windowSize).sum

    @tailrec
    def rec(ds: Seq[Int], counter: Int = 0): Int = {
      ds match {
        case Nil =>
          counter
        case _ :: tail if tail.size >= windowSize && sumOfFirstWindow(tail) > sumOfFirstWindow(ds) =>
          rec(tail, counter + 1)
        case _ :: tail =>
          rec(tail, counter)
      }
    }

    rec(depths)
  }
}
