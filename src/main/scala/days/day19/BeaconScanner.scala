package days.day19

import utils.FileReader.fileReader

import scala.annotation.tailrec

object BeaconScanner {

  case class Beacon(x: Int, y: Int, z: Int)
  case class Scanner(number: Int, beacons: Seq[Beacon])

  case class BeaconPair(scannerNumber: Int, beacons: (Beacon, Beacon)) {
    val dx: Int = Math.abs(beacons._1.x - beacons._2.x)
    val dy: Int = Math.abs(beacons._1.y - beacons._2.y)
    val dz: Int = Math.abs(beacons._1.z - beacons._2.z)
  }

  def run(): Unit = {
    val file = "/day-14-input.txt"
    println(s"Day 19 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Int = {
    val scanners = initialiseScanners(file)
    val beaconPairs = getBeaconPairs(scanners)
    val sharedBeacons = getSharedBeacons(0, 1, beaconPairs)
    println("***")
    println(sharedBeacons)
    ???
  }

  private def initialiseScanners(file: String): Seq[Scanner] = {
    val lines = fileReader(file)

    @tailrec
    def sRec(ls: Seq[String], acc: Seq[Scanner] = Nil): Seq[Scanner] = {
      ls match {
        case Nil => acc
        case head :: tail =>
          val pattern = "--- scanner ([0-9]+) ---".r
          val pattern(number) = head
          val (beacons, remainder) = bRec(tail)
          sRec(remainder, acc :+ Scanner(number.toInt, beacons))
      }
    }

    @tailrec
    def bRec(ls: Seq[String], acc: Seq[Beacon] = Nil): (Seq[Beacon], Seq[String]) = {
      ls match {
        case Nil => (acc, Nil)
        case head :: tail if head.trim.isEmpty => (acc, tail)
        case head :: tail =>
          val values = head.split(",").map(_.toInt)
          bRec(tail, acc :+ Beacon(values.head, values(1), values.last))
      }
    }

    sRec(lines)
  }

  private def getBeaconPairs(scanners: Seq[Scanner]): Seq[BeaconPair] = {
    scanners.foldLeft[Seq[BeaconPair]](Nil)((acc, scanner) => {
      @tailrec
      def rec(x: Int = 0, y: Int = 0, pairs: Seq[BeaconPair] = acc): Seq[BeaconPair] = {
        if (x == scanner.beacons.length - 1 && y == scanner.beacons.length - 1) {
          pairs
        } else if (y == scanner.beacons.length - 1) {
          rec(x + 1, x + 1, pairs :+ BeaconPair(scanner.number, (scanner.beacons(x), scanner.beacons(y))))
        } else {
          rec(x, y + 1, pairs :+ BeaconPair(scanner.number, (scanner.beacons(x), scanner.beacons(y))))
        }
      }

      rec()
    })
  }

  private def tomato(scanners: Seq[Scanner]) = {
    Seq.tabulate(0, scanners.length - 1) {
      (x, y) =>
      // if x == y
      // get shared beacons between scanner x and scanner y
      // if they share 12 or more beacons
      // figure it out
    }
  }

  private def getSharedBeacons(scanner1: Int, scanner2: Int, beaconPairs: Seq[BeaconPair]) = {
    beaconPairs
      .groupMap(x => (x.dx, x.dy, x.dz))(x => x.beacons)
  }

}
