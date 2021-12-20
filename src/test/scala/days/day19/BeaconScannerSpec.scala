package days.day19

import days.day19.BeaconScanner.part1
import org.scalatest.freespec.AnyFreeSpec

class BeaconScannerSpec extends AnyFreeSpec {

  private val file = "/day-19-test-input.txt"

  "part1" - {
    "when given example input" - {
      "must return 79" in {
        val result = part1(file)
        assert(result == 79)
      }
    }
  }

}
