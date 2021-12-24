package days.day24

import utils.FileReader.fileReader

import scala.annotation.tailrec
import scala.collection.mutable

object ArithmeticLogicUnit {

  def run(): Unit = {
    val file = "/day-24-input.txt"
    println(s"Day 24 part 1 result: ${run(file)}")
    println()
  }

  private def run(file: String): Long = {
    @tailrec
    def rec(modelNumber: Long = 99999999999999L): Long = {
      if (modelNumber.toString.contains('0')) {
        rec(modelNumber - 1)
      } else {
        if (isModelNumberValid(modelNumber, file)) {
          modelNumber
        } else {
          rec(modelNumber - 1)
        }
      }
    }
    rec()
  }

  private def isModelNumberValid(modelNumber: Long, file: String): Boolean = {
    val wxyz = mutable.Map[String, Int]()
    wxyz ++= List("w" -> 0, "x" -> 0, "y" -> 0, "z" -> 0)

    @tailrec
    def rec(inputs: Seq[Int], instructions: Seq[String]): Boolean = {
      instructions match {
        case Nil =>
          wxyz.get("z").contains(0)
        case head :: tail =>
          val args = head.split(" ")
          if (args(0) == "inp") {
            wxyz.update(args(1), inputs.head)
            rec(inputs.tail, tail)
          } else {
            val a = wxyz.getOrElse(args(1), 0)
            val b = args(2).toIntOption match {
              case Some(value) => value
              case None => wxyz.getOrElse(args(2), 0)
            }
            args(0) match {
              case "add" =>
                wxyz.update(args(1), a + b)
                rec(inputs, tail)
              case "mul" =>
                wxyz.update(args(1), a * b)
                rec(inputs, tail)
              case "div" =>
                if (b == 0) {
                  false
                } else {
                  wxyz.update(args(1), Math.floor(a.toFloat / b).toInt)
                  rec(inputs, tail)
                }
              case "mod" =>
                if (a < 0 || b <= 0) {
                  false
                } else {
                  wxyz.update(args(1), a % b)
                  rec(inputs, tail)
                }
              case "eql" =>
                wxyz.update(args(1), if (a == b) 1 else 0)
                rec(inputs, tail)
            }
          }
      }
    }

    rec(modelNumber.toString.map(_.toInt), fileReader(file))
  }

}
