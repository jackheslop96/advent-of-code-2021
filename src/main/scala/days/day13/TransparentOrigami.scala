package days.day13

import utils.FileReader.fileReader

import scala.annotation.tailrec

object TransparentOrigami {

  type Paper = Array[Array[Char]]

  case class Coordinate(x: Int, y: Int)

  object Coordinate {
    def apply(coordinates: String): Coordinate = {
      val xy = coordinates.split(",").map(_.toInt)
      new Coordinate(xy.head, xy.last)
    }
  }

  sealed trait Direction
  case object Up extends Direction
  case object Left extends Direction

  case class Fold(direction: Direction, line: Int)

  def run(): Unit = {
    val file = "/day-13-input.txt"
    println(s"Day 13 part 1 result: ${part1(file, 1)}")
    println("Day 13 part 2 result:")
    part2(file).foreach(x => println(x.mkString))
    println()
  }

  def part1(file: String, numberOfFolds: Int): Int = {
    val result = run(file)(_.take(numberOfFolds))
    result.flatten.count(_ == '#')
  }

  def part2(file: String): Paper =
    run(file)(identity)

  private def run(file: String)(f: Seq[String] => Seq[String]): Paper = {
    val input = fileReader(file)
    val dots = input.takeWhile(_.nonEmpty).map(Coordinate(_))
    val foldCommands = input.takeRight(input.size - dots.size - 1)
    val paper = initialisePaper(dots)
    f(foldCommands).foldLeft(paper)((acc, fc) => {
      foldPaper(acc, parseFoldCommand(fc))
    })
  }

  def initialisePaper(dots: Seq[Coordinate]): Paper = {
    val width = dots.map(_.x).max
    val length = dots.map(_.y).max
    val matrix = Array.fill(length + 1, width + 1)('.')
    dots.foreach { d =>
      matrix(d.y)(d.x) = '#'
    }
    matrix
  }

  def foldPaper(paper: Paper, fold: Fold): Paper = {

    def foldLine(cs: Array[Char]): Array[Char] = {

      def valueAtIndex(index: Int): Option[Char] =
        try Some(cs(index))
        catch {
          case _: IndexOutOfBoundsException => None
        }

      @tailrec
      def rec(acc: Array[Char] = Array.empty, counter: Int = 1): Array[Char] = {
        val c1 = valueAtIndex(fold.line - counter)
        val c2 = valueAtIndex(fold.line + counter)
        (c1, c2) match {
          case (None, None) => acc
          case _ => rec(acc :+ add(c1, c2), counter + 1)
        }
      }

      rec().reverse
    }

    fold.direction match {
      case Up =>
        paper.transpose.map(foldLine).transpose
      case Left =>
        paper.map(foldLine)
    }
  }

  private def add(c1: Option[Char], c2: Option[Char]): Char =
    if (c1.contains('#') || c2.contains('#')) '#' else '.'

  def parseFoldCommand(command: String): Fold = {
    val pattern = "fold along ([xy])=([0-9]+)".r
    val pattern(xy, line) = command
    val direction = xy match {
      case "x" => Left
      case "y" => Up
    }
    Fold(direction, line.toInt)
  }

}
