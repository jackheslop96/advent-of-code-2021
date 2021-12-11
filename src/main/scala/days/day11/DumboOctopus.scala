package days.day11

import utils.FileReader.fileReader

import scala.annotation.tailrec

case class DumboOctopus(energyLevel: Int, hasFlashedOnThisStep: Boolean) {

  def increaseEnergyLevel: DumboOctopus =
    energyLevel match {
      case _ if hasFlashedOnThisStep => this
      case x => this.copy(energyLevel = x + 1)
    }

  def flash: DumboOctopus = this.copy(energyLevel = 0, hasFlashedOnThisStep = true)

  def resetFlash: DumboOctopus = this.copy(hasFlashedOnThisStep = false)
}

object DumboOctopus {

  def apply(energyLevel: Int): DumboOctopus = new DumboOctopus(energyLevel, false)

  type Matrix = Array[Array[DumboOctopus]]

  case class MatrixAfterStep(matrix: Matrix, numberOfFlashes: Int)
  object MatrixAfterStep {
    def apply(matrix: Matrix): MatrixAfterStep = new MatrixAfterStep(matrix, 0)
  }

  def run(): Unit = {
    val file = "/day-11-input.txt"
    println(s"Day 11 part 1 result: ${part1(file, 100)}")
    println(s"Day 11 part 2 result: ${part2(file)}")
    println()
  }

  def part1(file: String, steps: Int): Int = {
    val matrix = initialiseMatrix(file)

    @tailrec
    def rec(steps: List[Int], matrix: Matrix, numberOfFlashes: Int = 0): Int = {
      steps match {
        case Nil => numberOfFlashes
        case _ :: tail =>
          val matrixAfterStep = step(matrix)
          rec(tail, matrixAfterStep.matrix, numberOfFlashes + matrixAfterStep.numberOfFlashes)
      }
    }

    rec((0 until steps).toList, matrix)
  }

  def part2(file: String): Int = {

    @tailrec
    def rec(matrix: Matrix, numberOfSteps: Int = 1): Int = {
      val matrixAfterStep = step(matrix)
      if (matrixAfterStep.matrix.flatten.forall(_.energyLevel == 0)) {
        numberOfSteps
      } else {
        rec(matrixAfterStep.matrix, numberOfSteps + 1)
      }
    }

    val matrix = initialiseMatrix(file)
    rec(matrix)
  }

  private def initialiseMatrix(file: String): Matrix =
    fileReader(file)
      .map(_.map(x => DumboOctopus(x.toString.toInt)).toArray)
      .toArray

  def step(matrix: Matrix): MatrixAfterStep = {

    @tailrec
    def rec(matrix: Matrix, numberOfFlashes: Int = 0): MatrixAfterStep = {
      var hasAnOctopusFlashed = false
      var updatedNumberOfFlashes = numberOfFlashes
      for (y <- matrix.indices; x <- matrix(y).indices) {
        if (matrix(y)(x).energyLevel > 9) {
          for (dx <- -1 to 1; dy <- -1 to 1) {
            if (dx == 0 && dy == 0) {
              matrix(y)(x) = matrix(y)(x).flash
              hasAnOctopusFlashed = true
              updatedNumberOfFlashes = updatedNumberOfFlashes + 1
            } else {
              try {
                matrix(y + dy)(x + dx) = matrix(y + dy)(x + dx).increaseEnergyLevel
              } catch {
                case _: ArrayIndexOutOfBoundsException => ()
              }
            }
          }
        }
      }

      if (hasAnOctopusFlashed) {
        rec(matrix, updatedNumberOfFlashes)
      } else {
        MatrixAfterStep(matrix, numberOfFlashes)
      }
    }

    val result = rec(matrix.map(_.map(_.increaseEnergyLevel)))
    MatrixAfterStep(
      matrix = result.matrix.map(_.map(_.resetFlash)),
      numberOfFlashes = result.numberOfFlashes
    )
  }

}
