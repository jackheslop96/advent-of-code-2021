import DepthIncreaseCounter._
import FileReader._
import Submarine._

object Main extends App {
  val depths = intFileReader("day-1-input.txt")
  println(s"Day 1 part 1 result: ${countIncrements(depths)}")
  println(s"Day 1 part 2 result: ${countIncrements(depths, 3)}")

  val instructions = stringFileReader("day-2-input.txt")
  println(s"Day 2 part 2 result: ${applyInstructions(instructions).multiplyPositions}")
}
