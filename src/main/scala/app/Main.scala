package app

import bingo.Bingo._
import submarine.DepthIncreaseCounter._
import submarine.DiagnosticReport._
import submarine.Submarine._
import utils.FileReader._
import vent.HydrothermalVent._

object Main extends App {
  val depths = intFileReader("/day-1-input.txt")
  println(s"Day 1 part 1 result: ${countIncrements(depths)}")
  println(s"Day 1 part 2 result: ${countIncrements(depths, 3)}")
  println()

  val instructions = stringFileReader("/day-2-input.txt")
  println(s"Day 2 part 2 result: ${applyInstructions(instructions).multiplyPositions}")
  println()

  val report = stringFileReader("/day-3-input.txt")
  println(s"Day 3 part 1 result: ${powerConsumption(report)}")
  println(s"Day 3 part 2 result: ${lifeSupportRating(report)}")
  println()

  val bingoConfig = stringFileReader("/day-4-input.txt")
  println("Day 4 result:")
  playGame(bingoConfig).foreach(x => println(s"${x._1}: ${x._2}"))
  println()

  val coordinates = stringFileReader("/day-5-input.txt")
  println(s"Day 5 part 2 result: ${countDangerousAreas(generateAllCoordinates(coordinates))}")
}
