import DiagnosticReport._
import org.scalatest.freespec.AnyFreeSpec

class DiagnosticReportSpec extends AnyFreeSpec {

  private val lines = Seq(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

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

  "binaryToDecimal" - {

    "when given 0" - {
      "must return 0" in {
        val result = binaryToDecimal("0")
        assert(result == 0)
      }
    }

    "when given 1" - {
      "must return 1" in {
        val result = binaryToDecimal("1")
        assert(result == 1)
      }
    }

    "when given 10" - {
      "must return 2" in {
        val result = binaryToDecimal("10")
        assert(result == 2)
      }
    }

    "when given 10110" - {
      "must return 22" in {
        val result = binaryToDecimal("10110")
        assert(result == 22)
      }
    }

    "when given 10111" - {
      "must return 23" in {
        val result = binaryToDecimal("10111")
        assert(result == 23)
      }
    }

    "when given 01010" - {
      "must return 10" in {
        val result = binaryToDecimal("01010")
        assert(result == 10)
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
