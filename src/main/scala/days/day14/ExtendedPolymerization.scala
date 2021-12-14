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
    val result = steps(template, insertions, numberOfSteps)
    val characterCounts = individualCharacterCount(result, template)
    val max = characterCounts.maxBy(_._2)
    val min = characterCounts.minBy(_._2)
    max._2 - min._2
  }

  def individualCharacterCount(map: Map[String, BigDecimal], template: String): Map[Char, BigDecimal] = {
    var acc: Map[Char, BigDecimal] = Map()
    map.foreach {
      case (str, l) =>
        val count = l / 2
        acc = str.toList match {
        case cs => combineMaps(acc, combineMaps(Map(cs.head -> count), Map(cs.last -> count)))
      }
    }
    acc = combineMaps(acc, Map(template.head -> 0.5))
    acc = combineMaps(acc, Map(template.last -> 0.5))
    acc
  }

  private def steps(template: String, insertions: Map[String, Char], numberOfSteps: Int): Map[String, BigDecimal] = {
    val initialCharPairs = groupStringIntoCharPairs(template)
    (1 to numberOfSteps).foldLeft[Map[String, BigDecimal]](initialCharPairs)((acc, _) => {
      step(acc, insertions)
    })
  }

  private def step(charPairs: Map[String, BigDecimal], insertions: Map[String, Char]): Map[String, BigDecimal] = {

    var acc: Map[String, BigDecimal] = charPairs

    charPairs.foreach {
      case (str, l) if l > 0 => insertions.get(str) match {
        case Some(value) => acc = combineMaps(
          combineMaps(acc, Map(str -> -l)),
          groupStringIntoCharPairs(s"${str.head}$value${str.last}", l)
        )
        case None => ()
      }
      case _ => ()
    }

    acc
  }

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
    a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0L)) }

}
