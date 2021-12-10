package days.day10

import utils.FileReader.fileReader

import scala.annotation.tailrec

object SyntaxScoring {

  case class Line(line: String, status: Status, charsOfInterest: List[Char])

  sealed trait Status
  case object Complete extends Status
  case object Incomplete extends Status
  case object Corrupted extends Status

  def run(): Unit = {
    val file = "/day-10-input.txt"
    println(s"Day 10 part 1 result: ${part1(file)}")
    println(s"Day 10 part 2 result: ${part2(file)}")
    println()
  }

  def part1(file: String): Int = {
    @tailrec
    def getPoints(chars: List[Char]): Int = {
      chars match {
        case Nil => 0
        case head :: _ if head == ')' => 3
        case head :: _ if head == ']' => 57
        case head :: _ if head == '}' => 1197
        case head :: _ if head == '>' => 25137
        case _ :: tail => getPoints(tail)
      }
    }

    getLines(file)
      .filter(_.status == Corrupted)
      .map(x => getPoints(x.charsOfInterest))
      .sum
  }

  def part2(file: String): Long = {
    implicit class LongSeq(xs: Seq[Long]) {
      def middle: Long = xs.sortWith(_ < _)(xs.size / 2)
    }

    def inverseChars(chars: List[Char]): List[Char] =
      chars.reverse.map {
        case '(' => ')'
        case '[' => ']'
        case '{' => '}'
        case '<' => '>'
      }

    def getPoints(chars: List[Char]): Long =
      chars.foldLeft(0L)((acc, c) => {
        (acc * 5L) + (c match {
          case ')' => 1L
          case ']' => 2L
          case '}' => 3L
          case '>' => 4L
        })
      })

    getLines(file)
      .filter(_.status == Incomplete)
      .map(x => inverseChars(x.charsOfInterest))
      .map(getPoints)
      .middle
  }

  private def getLines(file: String): Seq[Line] = fileReader(file).map(getLine)

  def getLine(line: String): Line = {

    val openingBrackets = Seq('(', '[', '{', '<')

    @tailrec
    def rec1(chars: List[Char]): Line = {
      chars match {
        case Nil => Line(line, Complete, Nil)
        case _ if chars.forall(openingBrackets.contains(_)) => Line(line, Incomplete, chars)
        case _ =>
          @tailrec
          def rec2(cs: List[Char], acc: List[Char] = Nil): List[Char] = {
            cs match {
              case Nil => acc
              case '(' :: ')' :: tail => rec2(tail, acc)
              case '[' :: ']' :: tail => rec2(tail, acc)
              case '{' :: '}' :: tail => rec2(tail, acc)
              case '<' :: '>' :: tail => rec2(tail, acc)
              case head :: tail => rec2(tail, acc :+ head)
            }
          }
          val updatedChars = rec2(chars)
          if (chars == updatedChars) {
            Line(line, Corrupted, chars)
          } else {
            rec1(updatedChars)
          }
      }
    }

    rec1(line.toList)
  }

}
