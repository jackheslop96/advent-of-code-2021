import FileReader.fileReader
import org.scalatest.freespec.AnyFreeSpec

class FileReaderSpec extends AnyFreeSpec {

  "fileReader" - {
    "must read a sequence of integers from a txt file" in {
      val file = "day-1-test-input.txt"
      lazy val result = fileReader(file)
      assertResult(Seq(1, 2, 3))(result)
    }
  }
}
