package days.day5

case class Coordinate(x: Int, y: Int)

object Coordinate {
  def apply(x: String, y: String): Coordinate = apply(x.toInt, y.toInt)
  def apply(x: Int, y: Int): Coordinate = new Coordinate(x, y)
}
