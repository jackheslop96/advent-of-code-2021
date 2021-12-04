package utils

import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader._

class FileReaderSpec extends AnyFreeSpec {

  "fileReader" - {
    "must read a sequence of integers from a txt file" in {
      val file = "/day-1-test-input.txt"
      lazy val result = intFileReader(file)
      assertResult(Seq(1, 2, 3))(result)
    }

    "must read a sequence of strings from a txt file" in {
      val file = "/day-2-test-input.txt"
      lazy val result = stringFileReader(file)
      assertResult(Seq("forward 1", "up 2", "down 3"))(result)
    }
  }
}
