package days.day16

import days.day16.PacketDecoder._
import org.scalatest.freespec.AnyFreeSpec

class PacketDecoderSpec extends AnyFreeSpec {

  "getPackets" - {
    "must return a list of packet strings" - {

      "when given 110100101111111000101000" in {
        val result = getPackets("110100101111111000101000")
        val expectedResult = Seq(
          Packet(6, 4)
        )
        assertResult(expectedResult)(result._1)
      }

      "when given 00111000000000000110111101000101001010010001001000000000" in {
        val result = getPackets("00111000000000000110111101000101001010010001001000000000")
        val expectedResult = Seq(
          Packet(1, 6, Seq(Packet(6, 4), Packet(2, 4)))
        )
        assertResult(expectedResult)(result._1)
      }

      "when given 11101110000000001101010000001100100000100011000001100000" in {
        val result = getPackets("11101110000000001101010000001100100000100011000001100000")
        val expectedResult = Seq(
          Packet(7, 3, Seq(Packet(2, 4), Packet(4, 4), Packet(1, 4)))
        )
        assertResult(expectedResult)(result._1)
      }
    }
  }

  "getSumOfVersions" - {

    "when given 8A004A801A8002F478" - {
      "must return 16" in {
        val result  = getSumOfVersions("8A004A801A8002F478")
        assert(result == 16)
      }
    }

    "when given 620080001611562C8802118E34" - {
      "must return 12" in {
        val result  = getSumOfVersions("620080001611562C8802118E34")
        assert(result == 12)
      }
    }

    "when given C0015000016115A2E0802F182340" - {
      "must return 23" in {
        val result  = getSumOfVersions("C0015000016115A2E0802F182340")
        assert(result == 23)
      }
    }

    "when given A0016C880162017C3686B18A3D4780" - {
      "must return 31" in {
        val result  = getSumOfVersions("A0016C880162017C3686B18A3D4780")
        assert(result == 31)
      }
    }
  }

}
