package days.day22

import days.day22.ReactorReboot._
import org.scalatest.freespec.AnyFreeSpec

class ReactorRebootSpec extends AnyFreeSpec {

  private val file = "/day-22-test-input.txt"

  "part1" - {
    "when given example input" - {
      "must return 590784" in {
        val result = part1(file)
        assert(result == 590784L)
      }
    }
  }

  "initialiseSteps" - {
    "when given 'on x=10..12,y=10..12,z=10..12'" - {
      "must create instruction to turn 27 cubes on" in {
        val result = initialiseInstruction("on x=10..12,y=10..12,z=10..12", areaConstraint = true).get
        assertResult(On)(result.state)
        assertResult(Cuboid((10, 12), (10, 12), (10,12)))(result.cuboid)
      }
    }

    "when given 'off x=12..10,y=12..10,z=12..10'" - {
      "must create instruction to turn 27 cubes on" in {
        val result = initialiseInstruction("off x=12..10,y=12..10,z=12..10", areaConstraint = true).get
        assertResult(Off)(result.state)
        assertResult(Cuboid((10, 12), (10, 12), (10,12)))(result.cuboid)
      }
    }

    "when given 'off x=53..55,y=53..55,z=53..55' and there is an area constraint" - {
      "must create instruction to turn 27 cubes on" in {
        val result = initialiseInstruction("off x=53..55,y=53..55,z=53..55", areaConstraint = false).get
        assertResult(Off)(result.state)
        assertResult(Cuboid((53, 55), (53, 55), (53,55)))(result.cuboid)
      }
    }

    "when given coordinate outside region 'x=-50..50,y=-50..50,z=-50..50' and there is an area constraint" - {
      "must not create instruction" in {
        val result = initialiseInstruction("on x=-54112..-39298,y=-85059..-49293,z=-27449..7877", areaConstraint = true)
        assertResult(None)(result)
      }
    }
  }

}
