package days.day22

import utils.FileReader.fileReader

import scala.collection.mutable

object ReactorReboot {

  sealed trait State
  case object On extends State
  case object Off extends State

  object State {
    def apply(state: String): State = state match {
      case "on" => On
      case "off" => Off
      case _ => throw new IllegalArgumentException
    }
  }

  type Coordinates = Seq[Coordinate]

  case class Coordinate(x: Int, y: Int, z: Int)
  case class Instruction(coordinates: Coordinates, state: State)

  def run(): Unit = {
    val file = "/day-22-input.txt"
    println(s"Day 22 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Long = {
    val instructions = initialiseInstructions(file)
    val states = mutable.Map[Coordinate, State]()

    for (instruction <- instructions) {
      instruction.coordinates.foreach { c =>
        states.update(c, instruction.state)
      }
    }

    states.filter(_._2 == On).keys.size
  }

  private def initialiseInstructions(file: String): Seq[Instruction] =
    fileReader(file).flatMap(initialiseInstruction)

  def initialiseInstruction(line: String): Option[Instruction] = {
    val pattern = "(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)".r
    val pattern(state, x1, x2, y1, y2, z1, z2) = line
    val xMin = Math.min(x1.toInt, x2.toInt); val xMax = Math.max(x1.toInt, x2.toInt)
    val yMin = Math.min(y1.toInt, y2.toInt); val yMax = Math.max(y1.toInt, y2.toInt)
    val zMin = Math.min(z1.toInt, z2.toInt); val zMax = Math.max(z1.toInt, z2.toInt)
    if (xMin >= -50 && xMax <= 50 && yMin >= -50 && yMax <= 50 && zMin >= -50 && zMax <= 50) {
      val coordinates = for (x <- xMin to xMax; y <- yMin to yMax; z <- zMin to zMax) yield Coordinate(x, y, z)
      Some(Instruction(coordinates, State(state)))
    } else {
      None
    }
  }

}
