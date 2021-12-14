package days.day14

import utils.FileReader.fileReader

import scala.annotation.tailrec

object ExtendedPolymerization {

  def run(): Unit = {
    val file = "/day-14-input.txt"
    println(s"Day 14 part 1 result: ${run(file, 10)}")
    println(s"Day 14 part 2 result: ${run(file, 40)}")
    println()
  }

  def run(file: String, numberOfSteps: Int): Long = {
    val input = fileReader(file)
    val template = input.head
    val insertions = input.tail.drop(1).foldLeft[Map[String, Char]](Map())((acc, insertion) => {
      val pattern = "([A-Z]+) -> ([A-Z])".r
      val pattern(s1, s2) = insertion
      acc + (s1 -> s2.head)
    })
    val counts = steps(template, insertions, numberOfSteps).groupBy(identity).map(_._2.length)
    counts.max - counts.min
  }

  private def steps(template: String, insertions: Map[String, Char], numberOfSteps: Int): List[Char] = {
    (1 to numberOfSteps).foldLeft[List[Char]](template.toList)((acc, _) => {
      step(acc, insertions)
    })
  }

  private def step(template: List[Char], insertions: Map[String, Char]): List[Char] = {
    @tailrec
    def rec(chars: List[Char], acc: List[Char] = Nil): List[Char] = {
      chars match {
        case Nil => acc
        case head :: next :: tail =>
          insertions.get(s"$head$next") match {
            case Some(value) => rec(next :: tail, acc :+ head :+ value)
            case None => ???
          }
        case head :: tail => rec(tail, acc :+ head)
      }
    }
    rec(template)
  }

}
