package days.day10

import utils.FileReader.fileReader

import scala.annotation.tailrec

object SyntaxScoring {

  def run(): Unit = {
    val file = "/day-10-input.txt"
    println(s"Day 10 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Int = {
    fileReader(file)
      .map(isLineValid)
      .map(_._2)
      .sum
  }

  def getCorruptedLines(file: String): Seq[String] = {
    fileReader(file).filter(x => !isLineValid(x)._1)
  }

  def isLineValid(line: String): (Boolean, Int) = {

    val openingBrackets = Seq('(', '[', '{', '<')

    @tailrec
    def rec1(chars: List[Char]): (Boolean, Int) = {
      chars match {
        case Nil => (true, 0)
        case _ if chars.forall(openingBrackets.contains(_)) => (true, 0)
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
            (false, getPoints(chars))
          } else {
            rec1(updatedChars)
          }
      }
    }

    rec1(line.toList)
  }

  @tailrec
  def getPoints(chars: Seq[Char]): Int = {
    chars match {
      case Nil => 0
      case head :: _ if head == ')' => 3
      case head :: _ if head == ']' => 57
      case head :: _ if head == '}' => 1197
      case head :: _ if head == '>' => 25137
      case _ :: tail => getPoints(tail)
    }
  }

}
