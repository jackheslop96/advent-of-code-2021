package days.day8

import utils.FileReader.fileReader

import scala.annotation.tailrec

object SevenSegmentSearch {

  def run(): Unit = {
    val file = "/day-8-input.txt"
    println(s"Day 8 part 1 result: ${part1(file)}")
    println(s"Day 8 part 2 result: ${part2(file)}")
  }

  def part1(file: String): Int = {
    val uniqueValues = Seq(1, 4, 7, 8)
    run(file).flatten.count(uniqueValues.contains(_))
  }

  def part2(file: String): Int =
    run(file).map(_.mkString.toInt).sum

  private def run(file: String): Seq[Seq[Int]] = {
    val lines = fileReader(file)
    lines.foldLeft[Seq[Seq[Int]]](Nil)((acc, line) => {
      val splitLine = line.split(" \\| ")
      val deducedNumbers = deduceNumbers(splitLine.head)
      acc :+ deduceOutput(splitLine.last, deducedNumbers)
    })
  }

  def deduceNumbers(line: String): Map[Int, String] = {
    // tries to deduce the number corresponding to the string at the front of the queue
    // if it can, it updates the map and removes that string from the queue
    // if it can't, it leaves the map as it is and sends the string to the back of the queue
    // the idea being that eventually it will have built up enough information to figure out each string
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

    rec(line.split(" ").toList)
  }

  // figures out 1, 4, 7 and 8
  // uses 1 to figure out 6
  // uses 4 to figure out 9 and 0
  // uses 7 to figure out 3
  // uses 6 to figure out 5 and 2
  def deduceNumber(string: String, map: Map[Int, String]): Option[Int] =
    string.length match {
      case 2 => Some(1)
      case 3 => Some(7)
      case 4 => Some(4)
      case 7 => Some(8)
      case 5 => map.get(7) flatMap {
        case value if isThreeString(string, value) => Some(3)
        case _ => map.get(6) map {
          case value if isFiveString(string, value) => 5
          case _ => 2
        }
      }
      case 6 => map.get(1) flatMap {
        case value if isSixString(string, value) => Some(6)
        case _ => map.get(4) map {
          case value if isNineString(string, value) => 9
          case _ => 0
        }
      }
    }

  // checks to see whether the characters in a shorter string are ALL contained within a longer string
  private def longerStringContainsAllCharactersInShorterString(longerString: String, shorterString: String): Boolean =
    shorterString.forall(c => longerString.contains(c))

  // used to deduce if a string corresponds to 5 by seeing if it is the same as the six string bar the 1 missing character
  // i.e.
  //      _         _
  //     |_   and  |_
  //      _|       |_|
  // are the same bar one character
  def isFiveString(stringToCheck: String, sixString: String): Boolean =
    sixString.length == stringToCheck.length + 1 &&
      longerStringContainsAllCharactersInShorterString(longerString = sixString, shorterString = stringToCheck)

  // uses the fact that the six string is the only string with 6 characters not to contain the one string
  private def isSixString(stringToCheck: String, oneString: String): Boolean =
    !longerStringContainsAllCharactersInShorterString(longerString = stringToCheck, shorterString = oneString)

  // uses the fact that the nine string is the only string with 6 characters to contain the four string
  private def isNineString(stringToCheck: String, fourString: String): Boolean =
    longerStringContainsAllCharactersInShorterString(longerString = stringToCheck, shorterString = fourString)

  // uses the fact that the three string is the only string with 5 characters to contain the seven string
  private def isThreeString(stringToCheck: String, sevenString: String): Boolean =
    longerStringContainsAllCharactersInShorterString(longerString = stringToCheck, shorterString = sevenString)

  // gets the number that each string corresponds to
  def deduceOutput(line: String, map: Map[Int, String]): Seq[Int] =
    line.split(" ").foldLeft[Seq[Int]](Nil)((acc, s) => {
      map.find(_._2.sorted == s.sorted) match {
        case Some((key, _)) => acc :+ key
        case _ => acc
      }
    })

}
