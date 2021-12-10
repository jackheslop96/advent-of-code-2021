package utils

import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader._

class FileReaderSpec extends AnyFreeSpec {

  "fileReader" - {
    "must read a .txt file" in {
      val file = "/day-01-test-input.txt"
      lazy val result = fileReader(file)
      val expectedResult = Seq("199", "200", "208", "210", "200", "207", "240", "269", "260", "263")
      assertResult(expectedResult)(result)
    }
  }
}
