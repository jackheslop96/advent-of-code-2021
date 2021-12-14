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

  def run(file: String, numberOfSteps: Int): BigDecimal = {
    val input = fileReader(file)
    val template = input.head
    val insertions = input.tail.drop(1).foldLeft[Map[String, Char]](Map())((acc, insertion) => {
      val pattern = "([A-Z]+) -> ([A-Z])".r
      val pattern(s1, s2) = insertion
      acc + (s1 -> s2.head)
    })
    val charPairs = steps(template, insertions, numberOfSteps)
    val charCounts = individualCharacterCounts(charPairs, template)
    charCounts.maxBy(_._2)._2 - charCounts.minBy(_._2)._2
  }

  def individualCharacterCounts(charPairs: Map[String, BigDecimal], template: String): Map[Char, BigDecimal] = {
    charPairs
      .foldLeft[Map[Char, BigDecimal]](Map())((acc, cp) => {
        cp match {
          case (str, bd) =>
            // since characters are shared between pairs we need to half the counts
            combineMaps(acc, combineMaps(Map(str.head -> bd/2), Map(str.last -> bd/2)))
        }
      })
      .map {
        case (k, v) =>
          // since the above would only denote a count of 0.5
          // to the first character in the first pair and last character in the last pair
          // we need to add an additional 0.5 for these characters
          if (k == template.head || k == template.last) (k, v + 0.5) else (k, v)
      }
  }

  private def steps(template: String, insertions: Map[String, Char], numberOfSteps: Int): Map[String, BigDecimal] =
    (1 to numberOfSteps).foldLeft[Map[String, BigDecimal]](groupStringIntoCharPairs(template))((acc, _) => {
      step(acc, insertions)
    })

  private def step(charPairs: Map[String, BigDecimal], insertions: Map[String, Char]): Map[String, BigDecimal] =
    charPairs.foldLeft[Map[String, BigDecimal]](charPairs)((acc, cp) => {
      cp match {
        case (str, bd) => insertions.get(str) match {
          case Some(char) =>
            // if we have NN -> C for example
            // we need to remove all of the NN char pairs
            // and add all of the newly created NC and CN char pairs
            combineMaps(
              combineMaps(acc, Map(str -> -bd)),
              groupStringIntoCharPairs(s"${str.head}$char${str.last}", bd)
            )
          case None => acc
        }
      }
    })

  private def groupStringIntoCharPairs(string: String, instances: BigDecimal = 1): Map[String, BigDecimal] = {

    @tailrec
    def rec(chars: List[Char], acc: Map[String, BigDecimal] = Map()): Map[String, BigDecimal] = {
      chars match {
        case head :: next :: tail => rec(next :: tail, combineMaps(acc, Map(s"$head$next" -> instances)))
        case _ => acc
      }
    }
    rec(string.toList)
  }

  def combineMaps[A](a: Map[A, BigDecimal], b: Map[A, BigDecimal]): Map[A, BigDecimal] =
    a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0)) }

}
