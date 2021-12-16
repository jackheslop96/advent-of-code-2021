package utils

import scala.math.pow

object BinaryUtils {

  def binaryToDecimal(binary: String): Double = binary
    .reverse
    .zipWithIndex
    .foldLeft[Double](0)((a, b) => (a, b) match {
      case (acc, (char, index)) =>
        char match {
          case '1' => acc + pow(2, index.toDouble)
          case _ => acc
        }
    })

  def hexadecimalToBinary(hexadecimal: String): String = {
    def convertChar(char: Char): String =
      char match {
        case '0' => "0000"
        case '1' => "0001"
        case '2' => "0010"
        case '3' => "0011"
        case '4' => "0100"
        case '5' => "0101"
        case '6' => "0110"
        case '7' => "0111"
        case '8' => "1000"
        case '9' => "1001"
        case 'A' => "1010"
        case 'B' => "1011"
        case 'C' => "1100"
        case 'D' => "1101"
        case 'E' => "1110"
        case 'F' => "1111"
        case _ => throw new IllegalArgumentException
      }

    hexadecimal.flatMap(convertChar)
  }
}
