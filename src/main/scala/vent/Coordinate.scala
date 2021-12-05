package vent

case class Coordinate(x: Int, y: Int)

object Coordinate {
  def apply(x: String, y: String): Coordinate = new Coordinate(x.toInt, y.toInt)
}
