package days.day8

import utils.FileReader.fileReader

import scala.annotation.tailrec

object SevenSegmentSearch {

  def run(): Unit = {
    val file = "/day-8-input.txt"
    println(s"Day 8 part 1 result: ${countOutputsWithUniqueNumberOfSegments(file)}")
    println(s"Day 8 part 2 result: ${run(file)}")
  }

  def run(file: String): Int = {
    val lines = fileReader(file)
    lines.foldLeft(0)((acc, line) => {
      val splitLine = line.split(" \\| ")
      val deducedNumbers = deduceNumbers(splitLine.head)
      val deducedOutput = deduceOutput(splitLine.last, deducedNumbers)
      acc + deducedOutput
    })
  }

  def countOutputsWithUniqueNumberOfSegments(file: String): Int = {
    val input = fileReader(file).map(x => x.split(" \\| ").last)
    val uniqueLengths = Seq(2, 3, 4, 7)

    @tailrec
    def rec(xs: Seq[String], count: Int = 0): Int = {
      xs match {
        case Nil => count
        case head :: tail =>
          val outputDigits = head.split(" ")
          rec(tail, count + outputDigits.count(od => uniqueLengths.contains(od.length)))
      }
    }

    rec(input)
  }

  def deduceNumbers(line: String): Map[Int, String] = {
    val strings = line.split(" ").toList

    @tailrec
    def rec(xs: List[String], acc: Map[Int, String] = Map()): Map[Int, String] = {
      xs match {
        case Nil => acc
        case head :: tail => deduceNumber(head, acc) match {
          case Some(key) => rec(tail, acc ++ Map(key -> head))
          case None => rec(tail :+ head, acc)
        }
      }
    }

    rec(strings)
  }

  // figures out 1, 4, 7 and 8
  // uses 1 to figure out 6
  // uses 4 to figure out 9 and 0
  // uses 7 to figure out 3
  // uses 6 to figure out 5 and 2
  def deduceNumber(string: String, map: Map[Int, String]): Option[Int] = {
    string match {
      case s if s.length == 2 => Some(1)
      case s if s.length == 3 => Some(7)
      case s if s.length == 4 => Some(4)
      case s if s.length == 7 => Some(8)
      case s if s.length == 5 =>
        map.get(7) flatMap {
          case value if string1ContainsAllString2Characters(s, value) => Some(3)
          case _ => map.get(6) map {
            case value if string2ContainsAllString1CharactersBarOne(s, value) => 5
            case _ => 2
          }
        }
      case s if s.length == 6 =>
        map.get(1) flatMap {
          case value if !string1ContainsAllString2Characters(s, value) => Some(6)
          case _ => map.get(4) map {
            case value if string1ContainsAllString2Characters(s, value) => 9
            case _ => 0
          }
        }
    }
  }

  private def string1ContainsAllString2Characters(string1: String, string2: String): Boolean = {
    string2.forall(c => string1.contains(c))
  }

  def string2ContainsAllString1CharactersBarOne(string1: String, string2: String): Boolean = {
    string2.length == string1.length + 1 && string1ContainsAllString2Characters(string2, string1)
  }

  def deduceOutput(line: String, map: Map[Int, String]): Int = {
    val sortedMap = map.map(kv => kv._1 -> kv._2.sorted)
    line.split(" ").foldLeft[Seq[String]](Nil)((acc, s) => {
      sortedMap.find(_._2 == s.sorted) match {
        case Some((key, _)) => acc :+ key.toString
        case _ => acc
      }
    }).mkString.toInt
  }

}
