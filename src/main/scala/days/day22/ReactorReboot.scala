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

  case class Coordinate(x: Int, y: Int, z: Int)

  case class Cuboid(xs: (Int, Int), ys: (Int, Int), zs: (Int, Int))

  case class Instruction(cuboid: Cuboid, state: State)

  def run(): Unit = {
    val file = "/day-22-input.txt"
    println(s"Day 22 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Long = run(file, areaConstraint = true)

  def part2(file: String): Long = run(file, areaConstraint = false)

  private def run(file: String, areaConstraint: Boolean): Long = {
    val instructions = initialiseInstructions(file, areaConstraint)
    val states = mutable.Map[Coordinate, State]()

    for {
      instruction <- instructions
      x <- instruction.cuboid.xs._1 to instruction.cuboid.xs._2
      y <- instruction.cuboid.ys._1 to instruction.cuboid.ys._2
      z <- instruction.cuboid.zs._1 to instruction.cuboid.zs._2
    } yield {
      states.update(Coordinate(x, y, z), instruction.state)
    }

    states.count(_._2 == On)
  }

  private def initialiseInstructions(file: String, areaConstraint: Boolean): Seq[Instruction] =
    fileReader(file).flatMap(initialiseInstruction(_, areaConstraint))

  def initialiseInstruction(line: String, areaConstraint: Boolean): Option[Instruction] = {
    val pattern = "(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)".r
    val pattern(state, x1, x2, y1, y2, z1, z2) = line
    val xMin = Math.min(x1.toInt, x2.toInt); val xMax = Math.max(x1.toInt, x2.toInt)
    val yMin = Math.min(y1.toInt, y2.toInt); val yMax = Math.max(y1.toInt, y2.toInt)
    val zMin = Math.min(z1.toInt, z2.toInt); val zMax = Math.max(z1.toInt, z2.toInt)
    if (!areaConstraint || (Seq(xMin, yMin, zMin).forall(_ >= -50) && Seq(xMax, yMax, zMax).forall(_ <= 50))) {
      Some(Instruction(Cuboid((xMin, xMax), (yMin, yMax), (zMin, zMax)), State(state)))
    } else {
      None
    }
  }

}
