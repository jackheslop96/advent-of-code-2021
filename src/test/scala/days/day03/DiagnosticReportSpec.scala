package days.day03

import days.day03.DiagnosticReport._
import org.scalatest.freespec.AnyFreeSpec
import utils.FileReader.fileReader

class DiagnosticReportSpec extends AnyFreeSpec {

  private val lines = fileReader("/day-03-test-input.txt")

  "gammaRate" - {
    "must return 10110" - {
      "when given the example report" in {
        val result = gammaRate(lines)
        assert(result == "10110")
      }
    }
  }

  "epsilonRate" - {
    "must return 01001" - {
      "when given the example report" in {
        val result = epsilonRate(lines)
        assert(result == "01001")
      }
    }
  }

  "powerConsumption" - {
    "when given the example report" - {
      "must return 198" in {
        val result = powerConsumption(lines)
        assert(result == 198)
      }
    }
  }

  "oxygenGeneratorRating" - {
    "when given the example report" - {
      "must return 10111" in {
        val result = oxygenGeneratorRating(lines)
        assert(result == "10111")
      }
    }
  }

  "co2ScrubberRating" - {
    "when given the example report" - {
      "must return 01010" in {
        val result = co2ScrubberRating(lines)
        assert(result == "01010")
      }
    }
  }

  "lifeSupportRating" - {
    "when given the example report" - {
      "must return 230" in {
        val result = lifeSupportRating(lines)
        assert(result == 230)
      }
    }
  }

  "mostCommon" - {

    "must return 0" - {
      "when there are more 0s than 1s" in {
        val seq = Seq('0', '0', '1')
        val result = mostCommon(seq)
        assert(result == '0')
      }
    }

    "must return 1" - {
      "when there are more 1s than 0s" in {
        val seq = Seq('0', '1', '1')
        val result = mostCommon(seq)
        assert(result == '1')
      }

      "when there are the same number of 1s and 0s" in {
        val seq = Seq('0', '1')
        val result = mostCommon(seq)
        assert(result == '1')
      }
    }
  }

  "leastCommon" - {

    "must return 0" - {
      "when there are less 0s than 1s" in {
        val seq = Seq('0', '1', '1')
        val result = leastCommon(seq)
        assert(result == '0')
      }

      "when there are the same number of 1s and 0s" in {
        val seq = Seq('0', '1')
        val result = leastCommon(seq)
        assert(result == '0')
      }
    }

    "must return 1" - {
      "when there are less 1s than 0s" in {
        val seq = Seq('0', '0', '1')
        val result = leastCommon(seq)
        assert(result == '1')
      }
    }
  }
}
