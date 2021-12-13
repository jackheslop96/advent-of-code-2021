package days.day13

import utils.FileReader.fileReader

object TransparentOrigami {

  type Paper = Seq[Seq[Char]]

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
    println()
  }

  def part1(file: String, numberOfFolds: Int): Int = {
    val input = fileReader(file)
    val dots = input.takeWhile(_.nonEmpty).map(Coordinate(_))
    val foldCommands = input.takeRight(input.size - dots.size - 1)
    val paper = initialisePaper(dots)
    val result = foldCommands.take(numberOfFolds).foldLeft(paper)((acc, fc) => {
      foldPaper(acc, parseFoldCommand(fc))
    })
    result.flatten.count(_ == '#')
  }

  def initialisePaper(dots: Seq[Coordinate]): Paper = {
    val width = dots.map(_.x).max
    val length = dots.map(_.y).max
    var paper: Paper = Nil
    for (y <- 0 to length) {
      var line: Seq[Char] = Nil
      for (x <- 0 to width) {
        line = line :+ (dots.find(d => d.x == x && d.y == y) match {
          case Some(_) => '#'
          case None => '.'
        })
      }
      paper = paper :+ line
    }
    paper
  }

  def foldPaper(paper: Paper, fold: Fold): Paper = {
    fold.direction match {
      case Up =>
        val topHalf = paper.take(fold.line)
        val bottomHalf = paper.takeRight(fold.line).reverse
        topHalf.zip(bottomHalf).map {
          case (value, value1) => value.zip(value1).map {
            case ('.', '.') => '.'
            case _ => '#'
          }
        }
      case Left =>
        val leftHalf = paper.map(_.take(fold.line))
        val rightHalf = paper.map(_.takeRight(fold.line).reverse)
        leftHalf.zip(rightHalf).map {
          case (value, value1) => value.zip(value1).map {
            case ('.', '.') => '.'
            case _ => '#'
          }
        }
    }
  }

  def parseFoldCommand(command: String): Fold = {
    val pattern = "fold along (x|y)=([0-9]+)".r
    val pattern(xy, line) = command
    val direction = xy match {
      case "x" => Left
      case "y" => Up
    }
    Fold(direction, line.toInt)
  }

}
