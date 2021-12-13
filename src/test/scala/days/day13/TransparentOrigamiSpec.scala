package days.day13

import days.day13.TransparentOrigami._
import org.scalatest.freespec.AnyFreeSpec

class TransparentOrigamiSpec extends AnyFreeSpec {

  private val file = "/day-13-test-input.txt"

  "part1" - {
    "must return 17" - {
      "when given example input" in {
        val result = part1(file, 1)
        assert(result == 17)
      }
    }
  }

  "initialisePaper" - {
    "must initialise paper" - {
      "when given a series of dots" in {
        val dots = Seq(
          "6,10",
          "0,14",
          "9,10",
          "0,3",
          "10,4",
          "4,11",
          "6,0",
          "6,12",
          "4,1",
          "0,13",
          "10,12",
          "3,4",
          "3,0",
          "8,4",
          "1,10",
          "2,14",
          "8,10",
          "9,0"
        ).map(Coordinate(_))

        val result = initialisePaper(dots)

        val expectedResult = Seq(
          Seq('.','.','.','#','.','.','#','.','.','#','.'),
          Seq('.','.','.','.','#','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('#','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','.','.','#','.','.','.','.','#','.','#'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','.','.','.','.','.'),
          Seq('.','#','.','.','.','.','#','.','#','#','.'),
          Seq('.','.','.','.','#','.','.','.','.','.','.'),
          Seq('.','.','.','.','.','.','#','.','.','.','#'),
          Seq('#','.','.','.','.','.','.','.','.','.','.'),
          Seq('#','.','#','.','.','.','.','.','.','.','.')
        )

        assertResult(expectedResult)(result)
      }
    }
  }

  "foldPaper" - {
    "must fold paper and combine dots/empties" - {

      val paper = Seq(
        Seq('.','#', '.'),
        Seq('.','.', '#'),
        Seq('#','.', '.')
      )

      "when folding up" in {
        val result = foldPaper(paper, Fold(Up, 2))
        val expectedResult = Seq(
          Seq('.','#', '.'),
          Seq('#','.', '#')
        )
        assertResult(expectedResult)(result)
      }

      "when folding left" in {
        val result = foldPaper(paper, Fold(Left, 2))
        val expectedResult = Seq(
          Seq('.', "#"),
          Seq('.', '#'),
          Seq('#', '.')
        )
        assertResult(expectedResult)(result)
      }
    }
  }

  "parseFoldCommand" - {

    "when given fold along y=7" - {
      "must return Fold(Up, 7)" in {
        val result = parseFoldCommand("fold along y=7")
        val expectedResult = Fold(Up, 7)
        assertResult(expectedResult)(result)
      }
    }

    "when given fold along x=5" - {
      "must return Fold(Left, 5)" in {
        val result = parseFoldCommand("fold along x=5")
        val expectedResult = Fold(Left, 5)
        assertResult(expectedResult)(result)
      }
    }
  }

  "part2" in {
    part2(file)
  }

}
