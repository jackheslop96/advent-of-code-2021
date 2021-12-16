package days.day16

import days.day16.PacketDecoder._
import org.scalatest.freespec.AnyFreeSpec

class PacketDecoderSpec extends AnyFreeSpec {

  "getPackets" - {
    "must return a list of packet strings" - {

      "when given 110100101111111000101000" in {
        val result = getPackets("110100101111111000101000")
        val expectedResult = Seq(
          Packet(6, 4, 2021)
        )
        assertResult(expectedResult)(result._1)
      }

      "when given 00111000000000000110111101000101001010010001001000000000" in {
        val result = getPackets("00111000000000000110111101000101001010010001001000000000")
        val expectedResult = Seq(
          Packet(1, 6, subPackets = Seq(Packet(6, 4, 10), Packet(2, 4, 20)))
        )
        assertResult(expectedResult)(result._1)
      }

      "when given 11101110000000001101010000001100100000100011000001100000" in {
        val result = getPackets("11101110000000001101010000001100100000100011000001100000")
        val expectedResult = Seq(
          Packet(7, 3, subPackets = Seq(Packet(2, 4, 1), Packet(4, 4, 2), Packet(1, 4, 3)))
        )
        assertResult(expectedResult)(result._1)
      }
    }
  }

  "part1" - {

    "when given 8A004A801A8002F478" - {
      "must return 16" in {
        val result  = part1("8A004A801A8002F478")
        assert(result == 16)
      }
    }

    "when given 620080001611562C8802118E34" - {
      "must return 12" in {
        val result  = part1("620080001611562C8802118E34")
        assert(result == 12)
      }
    }

    "when given C0015000016115A2E0802F182340" - {
      "must return 23" in {
        val result  = part1("C0015000016115A2E0802F182340")
        assert(result == 23)
      }
    }

    "when given A0016C880162017C3686B18A3D4780" - {
      "must return 31" in {
        val result  = part1("A0016C880162017C3686B18A3D4780")
        assert(result == 31)
      }
    }
  }

  "part2" - {

    "when given C200B40A82" - {
      "must return 3" in {
        val result = part2("C200B40A82")
        assert(result == 3)
      }
    }

    "when given 04005AC33890" - {
      "must return 54" in {
        val result = part2("04005AC33890")
        assert(result == 54)
      }
    }

    "when given 880086C3E88112" - {
      "must return 7" in {
        val result = part2("880086C3E88112")
        assert(result == 7)
      }
    }

    "when given CE00C43D881120" - {
      "must return 9" in {
        val result = part2("CE00C43D881120")
        assert(result == 9)
      }
    }

    "when given D8005AC2A8F0" - {
      "must return 1" in {
        val result = part2("D8005AC2A8F0")
        assert(result == 1)
      }
    }

    "when given F600BC2D8F" - {
      "must return 0" in {
        val result = part2("F600BC2D8F")
        assert(result == 0)
      }
    }

    "when given 9C005AC2F8F0" - {
      "must return 0" in {
        val result = part2("9C005AC2F8F0")
        assert(result == 0)
      }
    }

    "when given 9C0141080250320F1802104A08" - {
      "must return 1" in {
        val result = part2("9C0141080250320F1802104A08")
        assert(result == 1)
      }
    }
  }

}
