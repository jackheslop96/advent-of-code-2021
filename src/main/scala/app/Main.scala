package app

import bingo.Bingo._
import fish.LanternFish
import submarine.DepthIncreaseCounter._
import submarine.DiagnosticReport._
import submarine.Submarine._
import utils.FileReader._
import vent.HydrothermalVent

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
  println(s"Day 5 part 1 result: ${HydrothermalVent(coordinates, includeDiagonals = false)}")
  println(s"Day 5 part 2 result: ${HydrothermalVent(coordinates, includeDiagonals = true)}")
  println()

  println(s"Day 6 part 1 result: ${LanternFish.run("/day-6-input.txt", 80)}")
  println(s"Day 6 part 2 result: ${LanternFish.run("/day-6-input.txt", 256)}")
  println()
}
