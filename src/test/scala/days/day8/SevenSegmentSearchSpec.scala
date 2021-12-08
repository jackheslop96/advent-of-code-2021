package days.day8

import days.day8.SevenSegmentSearch._
import org.scalatest.freespec.AnyFreeSpec

class SevenSegmentSearchSpec extends AnyFreeSpec {

  private val file = "/day-8-test-input.txt"

  "countOutputsWithUniqueNumberOfSegments" - {
    "when given example input" - {
      "must return 26" in {
        val result = countOutputsWithUniqueNumberOfSegments(file)
        assert(result == 26)
      }
    }
  }

  "deduceNumber" - {

    "must return 7 for any string of length 3" in {
      val string = "dab"
      val result = deduceNumber(string, Map())
      assertResult(Some(7))(result)
    }

    "must return 1 for any string of length 2" in {
      val string = "ab"
      val result = deduceNumber(string, Map())
      assertResult(Some(1))(result)
    }

    "must return 8 for any string of length 7" in {
      val string = "acedgfb"
      val result = deduceNumber(string, Map())
      assertResult(Some(8))(result)
    }

    "must return 4 for any string of length 4" in {
      val string = "eafb"
      val result = deduceNumber(string, Map())
      assertResult(Some(4))(result)
    }

    "must return None for any string of length 5 when map is empty" in {
      val string = "gcdfa"
      val result = deduceNumber(string, Map())
      assertResult(None)(result)
    }

    "must return None for any string of length 6 when map is empty" in {
      val string = "cdfgeb"
      val result = deduceNumber(string, Map())
      assertResult(None)(result)
    }

    "must return 3 for a string of length 5 that contains the same characters as the 7 string" in {
      val string = "fbcad"
      val map = Map(7 -> "dab")
      val result = deduceNumber(string, map)
      assertResult(Some(3))(result)
    }

    "must return 6 for a string of length 6 that doesn't contain the same characters as the 1 string" in {
      val string = "cdfgeb"
      val map = Map(1 -> "ab")
      val result = deduceNumber(string, map)
      assertResult(Some(6))(result)
    }

    "must return 9 for a string of length 6 that contains the same characters as the 4 string" in {
      val string = "cefabd"
      val map = Map(4 -> "eafb", 1 -> "ab")
      val result = deduceNumber(string, map)
      assertResult(Some(9))(result)
    }

    "must return 0 for a string of length 6 that contains the same characters as the 1 string but not the 4 string" in {
      val string = "cagedb"
      val map = Map(1 -> "ab", 4 -> "eafb")
      val result = deduceNumber(string, map)
      assertResult(Some(0))(result)
    }

    "must return 5 for a string of length 5 that is only one character different from the 6 string" in {
      val string = "cdfbe"
      val map = Map(6 -> "cdfgeb", 7 -> "dab")
      val result = deduceNumber(string, map)
      assertResult(Some(5))(result)
    }

    "must return 2 for a string of length 5 that isn't only one character different from the 6 string" in {
      val string = "cdfbe"
      val map = Map(6 -> "gcdfa", 7 -> "dab")
      val result = deduceNumber(string, map)
      assertResult(Some(2))(result)
    }

    "must return None for the following example" in {
      val string = "ecdfab"
      val map = Map(7 -> "fgd", 8 -> "bdegcaf", 4 -> "fgec", 0 -> "aegbdf")
      val result = deduceNumber(string, map)
      assertResult(None)(result)
    }
  }

  "deduceNumbers" - {

    "when given acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab" - {
      "must return acedgfb: 8, cdfbe: 5, gcdfa: 2, fbcad: 3, dab: 7, cefabd: 9, cdfgeb: 6, eafb: 4, cagedb: 0, ab: 1" in {
        val input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
        val result = deduceNumbers(input)
        val expectedResult = Map(
          0 -> "cagedb",
          1 -> "ab",
          2 -> "gcdfa",
          3 -> "fbcad",
          4 -> "eafb",
          5 -> "cdfbe",
          6 -> "cdfgeb",
          7 -> "dab",
          8 -> "acedgfb",
          9 -> "cefabd"
        )
        assertResult(expectedResult)(result)
      }
    }

    "when given dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf" - {
      "must return bdegcaf: 8, fbedc: 5, dacgb: 2, dbcfg: 3, fgd: 7, gdcebf: 9, ecdfab: 6, fgec: 4, aegbdf: 0, gf: 1" in {
        val input = "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf"
        val result = deduceNumbers(input)
        val expectedResult = Map(
          0 -> "aegbdf",
          1 -> "gf",
          2 -> "dacgb",
          3 -> "dbcfg",
          4 -> "fgec",
          5 -> "fbedc",
          6 -> "ecdfab",
          7 -> "fgd",
          8 -> "bdegcaf",
          9 -> "gdcebf"
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "string2ContainsAllString1CharactersBarOne" - {
    "must return true" - {

      "when given abcd and abcde" in {
        val result = string2ContainsAllString1CharactersBarOne("abcd", "abcde")
        assertResult(true)(result)
      }

      "when given abc and dbca" in {
        val result = string2ContainsAllString1CharactersBarOne("abc", "dbca")
        assertResult(true)(result)
      }

      "when given cdfbe and cdfgeb" in {
        val result = string2ContainsAllString1CharactersBarOne("cdfbe", "cdfgeb")
        assertResult(true)(result)
      }
    }

    "must return false" - {

      "when given foo and bar" in {
        val result = string2ContainsAllString1CharactersBarOne("foo", "bar")
        assertResult(false)(result)
      }

      "when given abc and abde" in {
        val result = string2ContainsAllString1CharactersBarOne("abc", "abde")
        assertResult(false)(result)
      }

      "when given abc and abcde" in {
        val result = string2ContainsAllString1CharactersBarOne("abc", "abcde")
        assertResult(false)(result)
      }
    }
  }

  "deduceOutput" - {
    "when given cdfeb fcadb cdfeb cdbaf" - {
      "must return 5353" in {
        val map = Map(
          0 -> "cagedb",
          1 -> "ab",
          2 -> "gcdfa",
          3 -> "fbcad",
          4 -> "eafb",
          5 -> "cdfbe",
          6 -> "cdfgeb",
          7 -> "dab",
          8 -> "acedgfb",
          9 -> "cefabd"
        )
        val result = deduceOutput("cdfeb fcadb cdfeb cdbaf", map)
        assert(result == 5353)
      }
    }
  }

  "run" - {
    "when given the example input" - {
      "must return 61229" in {
        val result = SevenSegmentSearch.run(file)
        assert(result == 61229)
      }
    }
  }
}
