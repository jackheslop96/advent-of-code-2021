package vent

object HydrothermalVent {

  def countDangerousAreas(coordinates: Seq[Coordinate]): Int =
    coordinates
      .groupBy(identity)
      .count(_._2.size > 1)

  def generateAllCoordinates(lines: Seq[String]): Seq[Coordinate] =
    lines
      .filter(isLineHorizontalOrVertical)
      .flatMap(generateListOfCoordinates)

  def isLineHorizontalOrVertical(coordinatePair: String): Boolean = {
    val coordinates = parseCoordinatePair(coordinatePair)
    coordinates._1.x == coordinates._2.x || coordinates._1.y == coordinates._2.y
  }

  sealed trait CoordinateVariation
  case object ChangingInX extends CoordinateVariation
  case object ChangingInY extends CoordinateVariation

  def generateListOfCoordinates(coordinatePair: String): Seq[Coordinate] = {
    def generate(constantCoordinate: Int, coordinateRange: Range, variation: CoordinateVariation): Seq[Coordinate] = {
      coordinateRange.foldLeft[Seq[Coordinate]](Nil)((acc, coordinate) => {
        acc :+ (variation match {
          case ChangingInX => Coordinate(coordinate, constantCoordinate)
          case ChangingInY => Coordinate(constantCoordinate, coordinate)
        })
      })
    }

    Range(5, 0).foreach(println)

    parseCoordinatePair(coordinatePair) match {
      case (f, t) if f.x == t.x =>
        val step = if (f.y < t.y) 1 else -1
        generate(f.x, Range.inclusive(f.y, t.y, step), ChangingInY)
      case (f, t) if f.y == t.y =>
        val step = if (f.x < t.x) 1 else -1
        generate(f.y, Range.inclusive(f.x, t.x, step), ChangingInX)
      case _ => ???
    }
  }

  private def parseCoordinatePair(coordinatePair: String): (Coordinate, Coordinate) = {
    val pattern = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    val pattern(x1, y1, x2, y2) = coordinatePair
    val fromCoordinate = Coordinate(x1, y1)
    val toCoordinate = Coordinate(x2, y2)
    (fromCoordinate, toCoordinate)
  }

}
