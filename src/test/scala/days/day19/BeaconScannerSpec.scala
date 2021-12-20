package days.day19

import days.day19.BeaconScanner._
import org.scalatest.freespec.AnyFreeSpec

class BeaconScannerSpec extends AnyFreeSpec {

  private val file = "/day-19-test-input.txt"

  "initialiseScanners" - {
    "must initialise scanners" - {
      "when example input" in {
        val result = initialiseScanners(file)
        assert(result.size == 5)
        assert(result.map(_.beacons.size) == Seq(25, 25, 26, 25, 26))
      }
    }
  }

}
