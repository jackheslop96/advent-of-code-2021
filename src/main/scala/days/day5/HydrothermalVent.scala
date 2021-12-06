package days.day5

import utils.FileReader.fileReader

object HydrothermalVent {

  def run(): Unit = {
    val file = "/day-5-input.txt"
    println(s"Day 5 part 1 result: ${run(file, includeDiagonals = false)}")
    println(s"Day 5 part 2 result: ${run(file, includeDiagonals = true)}")
    println()
  }

  def run(file: String, includeDiagonals: Boolean): Int = {
    val input = fileReader(file)
    countDangerousAreas(generateAllCoordinates(input, includeDiagonals))
  }

  def countDangerousAreas(coordinates: Seq[Coordinate]): Int =
    coordinates
      .groupBy(identity)
      .count(_._2.size > 1)

  def generateAllCoordinates(lines: Seq[String], includeDiagonals: Boolean): Seq[Coordinate] =
    lines
      .filter(x => includeDiagonals || isLineHorizontalOrVertical(x))
      .flatMap(generateListOfCoordinates)

  def isLineHorizontalOrVertical(coordinatePair: String): Boolean = {
    val coordinates = parseCoordinatePair(coordinatePair)
    coordinates._1.x == coordinates._2.x || coordinates._1.y == coordinates._2.y
  }

  def generateListOfCoordinates(coordinatePair: String): Seq[Coordinate] = {
    def generate(xCoordinateRange: Range, yCoordinateRange: Range): Seq[Coordinate] = {
      val (xList, yList) = (xCoordinateRange.toList, yCoordinateRange.toList) match {
        case (xs, ys) if xs.size == 1 => (List.fill(ys.size)(xs.head), ys)
        case (xs, ys) if ys.size == 1 => (xs, List.fill(xs.size)(ys.head))
        case (xs, ys) => (xs, ys)
      }

      xList
        .zip(yList)
        .map(c => Coordinate(c._1, c._2))
    }

    parseCoordinatePair(coordinatePair) match {
      case (f, t) => generate(
        xCoordinateRange = Range.inclusive(f.x, t.x, step(f.x, t.x)),
        yCoordinateRange = Range.inclusive(f.y, t.y, step(f.y, t.y))
      )
    }
  }

  private def step(c1: Int, c2: Int): Int = if (c1 < c2) 1 else -1

  private def parseCoordinatePair(coordinatePair: String): (Coordinate, Coordinate) = {
    val pattern = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    val pattern(x1, y1, x2, y2) = coordinatePair
    (Coordinate(x1, y1), Coordinate(x2, y2))
  }

}
