package utils

import org.scalatest.freespec.AnyFreeSpec
import utils.BinaryUtils._

class BinaryUtilsSpec extends AnyFreeSpec {

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

  "hexadecimalToBinary" - {

    "when given 0" - {
      "must return 0000" in {
        val result = hexadecimalToBinary("0")
        assertResult("0000")(result)
      }
    }

    "when given 1" - {
      "must return 0001" in {
        val result = hexadecimalToBinary("1")
        assertResult("0001")(result)
      }
    }

    "when given F" - {
      "must return 1111" in {
        val result = hexadecimalToBinary("F")
        assertResult("1111")(result)
      }
    }

    "when given D2FE28" - {
      "must return 110100101111111000101000" in {
        val result = hexadecimalToBinary("D2FE28")
        assertResult("110100101111111000101000")(result)
      }
    }

  }

}
