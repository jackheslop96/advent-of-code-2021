package days.day10

import days.day10.SyntaxScoring._
import org.scalatest.freespec.AnyFreeSpec

class SyntaxScoringSpec extends AnyFreeSpec {

  private val file = "/day-10-test-input.txt"

  "getCorruptedLines" - {
    "must return the corrupted lines" - {
      "when given the example input" in {
        val result = getCorruptedLines(file)
        val expectedResult = Seq(
          "{([(<{}[<>[]}>{[]{[(<()>",
          "[[<[([]))<([[{}[[()]]]",
          "[{[{({}]{}}([{[{{{}}([]",
          "[<(<(<(<{}))><([]([]()",
          "<{([([[(<>()){}]>(<<{{"
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "isLineValid" - {
    "should return true" - {
      "when line is (" in {
        val result = isLineValid("(")._1
        assertResult(true)(result)
      }
      "when line is ()" in {
        val result = isLineValid("()")._1
        assertResult(true)(result)
      }
      "when line is {()()()}" in {
        val result = isLineValid("{()()()}")._1
        assertResult(true)(result)
      }
      "when line is <([{}])>" in {
        val result = isLineValid("<([{}])>")._1
        assertResult(true)(result)
      }
      "when line is [<>({}){}[([])<>]]" in {
        val result = isLineValid("[<>({}){}[([])<>]]")._1
        assertResult(true)(result)
      }
      "when line is (((((((((())))))))))" in {
        val result = isLineValid("(((((((((())))))))))")._1
        assertResult(true)(result)
      }
    }
    "should return false" - {
      "when line is (]" in {
        val result = isLineValid("(]")._1
        assertResult(false)(result)
      }
      "when line is {()()()>" in {
        val result = isLineValid("{()()()>")._1
        assertResult(false)(result)
      }
      "when line is (((()))}" in {
        val result = isLineValid("(((()))}")._1
        assertResult(false)(result)
      }
      "when line is <([]){()}[{}])" in {
        val result = isLineValid("<([]){()}[{}])")._1
        assertResult(false)(result)
      }
      "when line is {([(<{}[<>[]}>{[]{[(<()>" in {
        val result = isLineValid("{([(<{}[<>[]}>{[]{[(<()>")._1
        assertResult(false)(result)
      }
      "when line is [[<[([]))<([[{}[[()]]]" in {
        val result = isLineValid("[[<[([]))<([[{}[[()]]]")._1
        assertResult(false)(result)
      }
      "when line is [{[{({}]{}}([{[{{{}}([]" in {
        val result = isLineValid("[{[{({}]{}}([{[{{{}}([]")._1
        assertResult(false)(result)
      }
      "when line is [<(<(<(<{}))><([]([]()" in {
        val result = isLineValid("[<(<(<(<{}))><([]([]()")._1
        assertResult(false)(result)
      }
      "when line is <{([([[(<>()){}]>(<<{{" in {
        val result = isLineValid("<{([([[(<>()){}]>(<<{{")._1
        assertResult(false)(result)
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

}
