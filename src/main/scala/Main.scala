import DepthIncreaseCounter.countIncrements
import FileReader.fileReader

object Main extends App {
  val depths = fileReader("day-1-input.txt")
  val result = countIncrements(depths)
  println(s"Result: $result")
}
