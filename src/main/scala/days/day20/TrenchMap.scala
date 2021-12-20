package days.day20

import utils.BinaryUtils.binaryToDecimal
import utils.FileReader.fileReader

import scala.annotation.tailrec

object TrenchMap {

  type Matrix = Array[Array[Char]]

  case class TrenchFloor(algorithm: String, image: Matrix)

  def run(): Unit = {
    val file = "/day-20-input.txt"
    println(s"Day 20 part 1 result: ${run(file, 2)}")
    println(s"Day 20 part 2 result: ${run(file, 50)}")
    println()
  }

  def run(file: String, numberOfSteps: Int): Int = {
    @tailrec
    def rec(trenchFloor: TrenchFloor, defaultChar: Char, counter: Int = 0): Int = {
      if (counter >= numberOfSteps) {
        trenchFloor.image.flatten.count(_ == '#')
      } else {
        val updatedTrench = applyAlgorithm(trenchFloor, defaultChar)
        val updatedDefaultChar = binaryToChar(charsToBinary(Seq.fill(9)(defaultChar)), trenchFloor.algorithm)
        rec(
          updatedTrench.copy(image = expandImage(updatedTrench.image, updatedDefaultChar)),
          updatedDefaultChar,
          counter + 1
        )
      }
    }

    val trenchFloor = initialise(file)
    rec(trenchFloor.copy(image = expandImage(trenchFloor.image)), '.')
  }

  def applyAlgorithm(trenchFloor: TrenchFloor, defaultChar: Char): TrenchFloor = {
    val newImage = Array.fill(trenchFloor.image.length, trenchFloor.image.head.length)('.')
    for (y <- trenchFloor.image.indices; x <- trenchFloor.image(y).indices) {

      def charToBinary: String = {
        var chars: Seq[Char] = Nil
        for (y2 <- y - 1 to y + 1; x2 <- x - 1 to x + 1) {
          try {
            chars = chars :+ trenchFloor.image(y2)(x2)
          } catch {
            case _: ArrayIndexOutOfBoundsException => chars = chars :+ defaultChar
          }
        }
        charsToBinary(chars)
      }

      newImage(y)(x) = binaryToChar(charToBinary, trenchFloor.algorithm)
    }

    trenchFloor.copy(image = newImage)
  }

  private def charsToBinary(chars: Seq[Char]): String = {
    chars.mkString.map {
      case '.' => '0'
      case _ => '1'
    }
  }

  private def binaryToChar(binary: String, algorithm: String): Char = {
    val index = binaryToDecimal(binary).toInt
    algorithm(index)
  }

  def expandImage(image: Matrix, defaultChar: Char = '.'): Matrix = {
    @tailrec
    def rec(image: Matrix): Matrix = {
      val hasTopRows = image.take(3).forall(_.forall(_ == defaultChar))
      val hasBottomRows = image.takeRight(3).forall(_.forall(_ == defaultChar))
      val hasLeftColumns = image.forall(_.take(3).forall(_ == defaultChar))
      val hasRightColumns = image.forall(_.takeRight(3).forall(_ == defaultChar))

      if (hasTopRows && hasBottomRows && hasLeftColumns && hasRightColumns) {
        image
      } else {
        lazy val width = image.head.length
        lazy val row = Array.fill(width)(defaultChar)
        if (!hasTopRows) {
          rec(Array(row) ++ image)
        } else if (!hasBottomRows) {
          rec(image ++ Array(row))
        } else if (!hasLeftColumns) {
          rec(image.map(x => Array(defaultChar) ++ x))
        } else {
          rec(image.map(_ ++ Array(defaultChar)))
        }
      }
    }
    rec(image)
  }

  private def initialise(file: String): TrenchFloor = {
    val input = fileReader(file)

    val image = input
      .drop(2)
      .map(_.toArray)
      .toArray

    TrenchFloor(input.head, image)
  }

}
