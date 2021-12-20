package days.day19

import utils.FileReader.fileReader

import scala.annotation.tailrec

// TODO
object BeaconScanner {

  case class Beacon(x: Int, y: Int, z: Int)

  case class Scanner(number: Int, beacons: Seq[Beacon])

  def initialiseScanners(file: String): Seq[Scanner] = {
    @tailrec
    def rec(ls: Seq[String], acc: Seq[Scanner] = Nil): Seq[Scanner] =
      ls match {
        case Nil => acc
        case head :: tail if head.isEmpty => rec(tail, acc)
        case head :: tail =>
          val pattern = "--- scanner ([0-9]+) ---".r
          val pattern(number) = head
          val beacons = tail.takeWhile(_.nonEmpty).map(x => x.split(",")).map(_.map(_.toInt)).map(x => Beacon(x(0), x(1), x(2)))
          rec(tail.dropWhile(_.nonEmpty), acc :+ Scanner(number.toInt, beacons))
      }

    rec(fileReader(file))
  }

}
