package days.day10

import days.day10.SyntaxScoring._
import org.scalatest.freespec.AnyFreeSpec

class SyntaxScoringSpec extends AnyFreeSpec {

  private val file = "/day-10-test-input.txt"

  "getLine" - {
    "should return Complete" - {
      "when line is ()" in {
        val result = getLine("()")
        assertResult(Complete)(result.status)
      }
      "when line is {()()()}" in {
        val result = getLine("{()()()}")
        assertResult(Complete)(result.status)
      }
      "when line is <([{}])>" in {
        val result = getLine("<([{}])>")
        assertResult(Complete)(result.status)
      }
      "when line is [<>({}){}[([])<>]]" in {
        val result = getLine("[<>({}){}[([])<>]]")
        assertResult(Complete)(result.status)
      }
      "when line is (((((((((())))))))))" in {
        val result = getLine("(((((((((())))))))))")
        assertResult(Complete)(result.status)
      }
    }
    "should return Incomplete" - {
      "when line is (" in {
        val result = getLine("(")
        assertResult(Incomplete)(result.status)
      }
    }
    "should return Corrupted" - {
      "when line is (]" in {
        val result = getLine("(]")
        assertResult(Corrupted)(result.status)
      }
      "when line is {()()()>" in {
        val result = getLine("{()()()>")
        assertResult(Corrupted)(result.status)
      }
      "when line is (((()))}" in {
        val result = getLine("(((()))}")
        assertResult(Corrupted)(result.status)
      }
      "when line is <([]){()}[{}])" in {
        val result = getLine("<([]){()}[{}])")
        assertResult(Corrupted)(result.status)
      }
      "when line is {([(<{}[<>[]}>{[]{[(<()>" in {
        val result = getLine("{([(<{}[<>[]}>{[]{[(<()>")
        assertResult(Corrupted)(result.status)
      }
      "when line is [[<[([]))<([[{}[[()]]]" in {
        val result = getLine("[[<[([]))<([[{}[[()]]]")
        assertResult(Corrupted)(result.status)
      }
      "when line is [{[{({}]{}}([{[{{{}}([]" in {
        val result = getLine("[{[{({}]{}}([{[{{{}}([]")
        assertResult(Corrupted)(result.status)
      }
      "when line is [<(<(<(<{}))><([]([]()" in {
        val result = getLine("[<(<(<(<{}))><([]([]()")
        assertResult(Corrupted)(result.status)
      }
      "when line is <{([([[(<>()){}]>(<<{{" in {
        val result = getLine("<{([([[(<>()){}]>(<<{{")
        assertResult(Corrupted)(result.status)
      }
    }
  }

  "part1" - {
    "should return 26397" - {
      "when given example input" in {
        val result = part1(file)
        assert(result == 26397)
      }
    }
  }

  "part2" - {
    "should return 288957" - {
      "when given example input" in {
        val result = part2(file)
        assert(result == 288957L)
      }
    }
  }

}
