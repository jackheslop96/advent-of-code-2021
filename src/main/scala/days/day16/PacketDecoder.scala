package days.day16

import utils.BinaryUtils.{binaryToDecimal, hexadecimalToBinary}
import utils.FileReader.fileReader

import scala.annotation.tailrec

object PacketDecoder {

  def run(): Unit = {
    val file = "/day-16-input.txt"
    println(s"Day 16 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Int =
    getSumOfVersions(fileReader(file).head)

  case class Packet(version: Int, typeId: Int, subPackets: Seq[Packet] = Nil)

  def getPackets(binary: String, max: Int = Int.MaxValue): (Seq[Packet], String) = {
    @tailrec
    def rec(binary: String, packets: Seq[Packet] = Nil): (Seq[Packet], String) = {
      getPacket(binary) match {
        case (packet, remainder) =>
          if (remainder.isEmpty || remainder.forall(_ == '0')) {
            (packets :+ packet, "")
          } else {
            if (packets.length + 1 >= max) {
              (packets :+ packet, remainder)
            } else {
              rec(remainder, packets :+ packet)
            }
          }
      }
    }
    rec(binary)
  }

  private def getPacket(binary: String): (Packet, String) = {
    val pattern = "([0-9]{3})([0-9]{3})([0-9]+)".r
    val pattern(v, t, rest) = binary
    val version = binaryToDecimal(v).toInt
    val typeId = binaryToDecimal(t).toInt
    typeId match {
      case 4 =>
        @tailrec
        def rec(str: String, acc: String = ""): (String, String) = {
          str.head match {
            case '0' => (acc + str.tail.take(4), str.drop(5))
            case _ => rec(str.drop(5), acc + str.tail.take(4))
          }
        }
        (Packet(version, typeId, Nil), rec(rest)._2)
      case _ =>
        rest.head match {
          case '0' =>
            //val totalLength = binaryToDecimal(rest.tail.take(15)).toInt
            val (subPackets, remainder) = getPackets(rest.tail.drop(15))
            (Packet(version, typeId, subPackets), remainder)
          case _ =>
            val numberOfSubPackets = binaryToDecimal(rest.tail.take(11)).toInt
            val (subPackets, remainder) = getPackets(rest.tail.drop(11), numberOfSubPackets)
            (Packet(version, typeId, subPackets), remainder)
        }

    }
  }

  def getSumOfVersions(input: String): Int = {
    def rec(packets: Seq[Packet], acc: Int = 0): Int = {
      packets match {
        case Nil => acc
        case head :: tail => rec(head.subPackets, head.version) + rec(tail, acc)
      }
    }
    rec(getPackets(hexadecimalToBinary(input))._1)
  }

}
