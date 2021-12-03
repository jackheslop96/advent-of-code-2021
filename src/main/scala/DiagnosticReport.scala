import scala.annotation.tailrec
import scala.math.pow

object DiagnosticReport {

  def gammaRate(lines: Seq[String]): String =
    aggregateLines(lines)(mostCommon)

  def epsilonRate(lines: Seq[String]): String =
    aggregateLines(lines)(leastCommon)

  def powerConsumption(lines: Seq[String]): Double =
    binaryToDecimal(gammaRate(lines)) * binaryToDecimal(epsilonRate(lines))

  private def aggregateLines(lines: Seq[String])(f: Seq[Char] => Char): String =
    lines.transpose.map(f).mkString

  def binaryToDecimal(binary: String): Double = binary.reverse.zipWithIndex.foldLeft[Double](0)((a, b) => {
    (a, b) match {
      case (acc, (char, index)) =>
        char match {
          case '1' => acc + pow(2, index.toDouble)
          case _ => acc
        }
    }
  })

  def oxygenGeneratorRating(lines: Seq[String]): String = {
    gasRating(lines)(mostCommon)
  }

  def co2ScrubberRating(lines: Seq[String]): String = {
    gasRating(lines)(leastCommon)
  }

  private def gasRating(lines: Seq[String])(f: Seq[Char] => Char): String = {
    @tailrec
    def rec(xs: Seq[String], index: Int = 0): String = {
      val transposedLines = xs.transpose
      xs match {
        case head :: Nil =>
          head
        case _ =>
          val filteredLines = xs.filter(_(index) == f(transposedLines(index)))
          rec(filteredLines, index + 1)
      }
    }
    rec(lines)
  }

  sealed trait Commonality
  case object MostCommon extends Commonality
  case object LeastCommon extends Commonality

  def mostCommon(chars: Seq[Char]): Char = commonality(chars, MostCommon)

  def leastCommon(chars: Seq[Char]): Char = commonality(chars, LeastCommon)

  private def commonality(chars: Seq[Char], commonality: Commonality): Char = {
    (chars.count(_ == '0'), chars.count(_ == '1'), commonality) match {
      case (x, y, LeastCommon) if x == y || x < y => '0'
      case (x, y, MostCommon) if x == y || x < y => '1'
      case (_, _, LeastCommon) => '1'
      case (_, _, MostCommon) => '0'
    }
  }

  def lifeSupportRating(lines: Seq[String]): Double =
    binaryToDecimal(oxygenGeneratorRating(lines)) * binaryToDecimal(co2ScrubberRating(lines))

}
