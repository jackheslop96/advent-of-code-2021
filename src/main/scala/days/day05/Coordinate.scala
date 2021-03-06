package days.day05

case class Coordinate(x: Int, y: Int)

object Coordinate {
  def apply(x: Int, y: Int): Coordinate = new Coordinate(x, y)
  def apply(x: String, y: String): Coordinate = apply(x.toInt, y.toInt)
}
