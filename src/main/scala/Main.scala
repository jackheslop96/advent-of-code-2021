import DepthIncreaseCounter._
import FileReader.fileReader

object Main extends App {
  val depths = fileReader("day-1-input.txt")

  println(s"Day 1 part 1 result: ${countIncrements(depths)}")
  println(s"Day 1 part 2 result: ${countIncrements(depths, 3)}")
}
