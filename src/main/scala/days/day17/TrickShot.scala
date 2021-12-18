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
      x > targetArea.x2 || (x >= targetArea.x1 && y < targetArea.y1)
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

  sealed trait Result
  case class Hit(highestY: Int) extends Result
  case object Undershoot extends Result
  case object Overshoot extends Result
  case object InfiniteUndershoot extends Result
  case object Through extends Result
  case object InfiniteThrough extends Result

  def run(): Unit = {
    val file = "/day-17-input.txt"
    println(s"Day 17 part 1 result: ${part1(file)}")
    println(s"Day 17 part 2 result: ${part2(file)}")
    println()
  }

  private def run(file: String): Seq[Hit] = {
    val targetArea = initialiseTargetArea(file)
    hits(targetArea)
  }

  def part1(file: String): Int = run(file).maxBy(_.highestY).highestY

  def part2(file: String): Int = run(file).size

  private def hits(targetArea: TargetArea): Seq[Hit] = {
    (minX(targetArea) to targetArea.x2).foldLeft[Seq[Hit]](Nil)((acc, x) => {
      @tailrec
      def rec(y: Int, hits: Seq[Hit] = Nil): Seq[Hit] = {
        steps(Probe().fire(Velocity(x, y)), targetArea) match {
          case hit: Hit => rec(y + 1, hits :+ hit)
          case Undershoot | Through => rec(y + 1, hits)
          case _ => hits
        }
      }
      acc ++ rec(targetArea.y1)
    })
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

  private def steps(probe: Probe, targetArea: TargetArea): Result = {

    @tailrec
    def rec(prev: Probe, highestY: Int = probe.position.y): Result = {
      prev.step match {
        case curr if curr.position.isInTargetArea(targetArea) =>
          Hit(Math.max(curr.position.y, highestY))
        case curr if curr.position.hasMissedTargetArea(targetArea) =>
          result(prev, curr, targetArea)
        case curr =>
          rec(curr, Math.max(curr.position.y, highestY))
      }
    }

    rec(probe)
  }

  private def result(prev: Probe, curr: Probe, targetArea: TargetArea): Result = {
    if (prev.position.x == curr.position.x) {
      if (prev.position.x >= targetArea.x1 && prev.position.x <= targetArea.x2) {
        // probe has gone through target
        if (prev.position.y == 0 || curr.position.y == 0) {
          // no increase in initial Y velocity will ever make the probe hit the target
          InfiniteThrough
        } else {
          Through
        }
      } else {
        // no increase in initial Y velocity will ever make the probe hit the target
        InfiniteUndershoot
      }
    } else {
      val gradient = (curr.position.y - prev.position.y).toFloat / (curr.position.x - prev.position.x)
      val yIntercept = curr.position.y - (gradient * curr.position.x)
      if ((gradient * targetArea.x2) + yIntercept > targetArea.y2.toFloat) Overshoot else Undershoot
    }
  }

  private def initialiseTargetArea(file: String): TargetArea = {
    val input = fileReader(file).head
    val pattern = "target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)".r
    val pattern(s1, s2, s3, s4) = input
    TargetArea(s1.toInt, s2.toInt, s3.toInt, s4.toInt)
  }

}
