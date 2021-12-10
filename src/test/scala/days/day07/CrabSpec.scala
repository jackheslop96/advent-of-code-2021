package days.day07

import org.scalatest.freespec.AnyFreeSpec

class CrabSpec extends AnyFreeSpec {

  private val file = "/day-07-test-input.txt"

  "when burning fuel at constant rate" - {
    "must return 37 for example input" in {
      val result = Crab.run(file, burnsFuelAtConstantRate = true)
      assert(result == 37)
    }
  }

  "when not burning fuel at constant rate" - {
    "must return 168 for example input" in {
      val result = Crab.run(file, burnsFuelAtConstantRate = false)
      assert(result == 168)
    }
  }

}
