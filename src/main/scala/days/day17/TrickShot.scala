package days.day17

import utils.FileReader.fileReader

import scala.annotation.tailrec

object TrickShot {

  case class TargetArea(x1: Int, x2: Int, y1: Int, y2: Int)

  case class Coordinate(x: Int, y: Int) {
    def step(velocity: Velocity): Coordinate = this.copy(x = x + velocity.x, y = y + velocity.y)

    def isInTargetArea(targetArea: TargetArea): Boolean =
      x >= targetArea.x1 && x <= targetArea.x2 && y >= targetArea.y1 && y <= targetArea.y2

    def hasMissedTargetArea(targetArea: TargetArea): Boolean =
      x > targetArea.x2 || y < targetArea.y1
  }

  case class Velocity(x: Int, y: Int) {
    def step: Velocity = this.copy(x = drag(x), y = y - 1)

    private def drag(velocity: Int): Int =
      if (velocity > 0) velocity - 1 else if (velocity < 0) velocity + 1 else velocity
  }

  case class Probe(position: Coordinate, velocity: Velocity) {
    def fire(velocity: Velocity): Probe = this.copy(velocity = velocity)
    def step: Probe = this.copy(position = position.step(velocity), velocity = velocity.step)
  }

  object Probe {
    def apply(): Probe = new Probe(Coordinate(0, 0), Velocity(0, 0))
  }

  def run(): Unit = {
    val file = "/day-17-input.txt"
    println(s"Day 17 part 1 result: ${part1(file)}")
    println(s"Day 17 part 2 result: ${part2(file)}")
    println()
  }

  private def run(file: String): Seq[Int] = {
    val targetArea = initialiseTargetArea(file)
    val probes = velocities(targetArea).map(Probe().fire(_))
    probes.flatMap(steps(_, targetArea))
  }

  def part1(file: String): Int = run(file).max

  def part2(file: String): Int = run(file).size

  private def velocities(targetArea: TargetArea): Seq[Velocity] = {
    var velocities: Seq[Velocity] = Nil
    val xs = minX(targetArea) to targetArea.x2
    val ys = targetArea.y1 to 200 // TODO - don't use arbitrary yMax
    for (x <- xs; y <- ys) {
      velocities = velocities :+ Velocity(x, y)
    }
    velocities
  }

  private def minX(targetArea: TargetArea): Int = {
    @tailrec
    def rec(x: Int = 0, acc: Int = 0): Int = {
      acc + x match {
        case r if r >= targetArea.x1 => x
        case r => rec(x + 1, r)
      }
    }
    rec()
  }

  private def steps(probe: Probe, targetArea: TargetArea): Option[Int] = {

    @tailrec
    def rec(probe: Probe, highestY: Int = probe.position.y): Option[Int] = {
      probe.step match {
        case Probe(position, _) if position.isInTargetArea(targetArea) => Some(Math.max(position.y, highestY))
        case Probe(position, _) if position.hasMissedTargetArea(targetArea) => None
        case probe => rec(probe, Math.max(probe.position.y, highestY))
      }
    }

    rec(probe)
  }

  private def initialiseTargetArea(file: String): TargetArea = {
    val input = fileReader(file).head
    val pattern = "target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)".r
    val pattern(s1, s2, s3, s4) = input
    TargetArea(s1.toInt, s2.toInt, s3.toInt, s4.toInt)
  }

}
