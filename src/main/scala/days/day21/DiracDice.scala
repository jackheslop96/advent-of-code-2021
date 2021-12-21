package days.day21

import utils.FileReader.fileReader

import scala.annotation.tailrec

object DiracDice {

  def run(): Unit = {
    val file = "/day-21-input.txt"
    println(s"Day 21 part 1 result: ${part1(file)}")
    println()
  }

  case class Player(number: Int, position: Int, points: Int, turns: Int) {
    def takeTurn(rolls: Seq[Int]): Player = {
      @tailrec
      def rec(rolls: Seq[Int], player: Player): Player = {
        rolls match {
          case Nil => player.copy(points = points + player.position, turns = turns + 1)
          case head :: tail =>
            val updatedPlayer = {
              val updatedPosition = (player.position + head) % 10 match {
                case 0 => 10
                case p => p
              }
              player.copy(position = updatedPosition)
            }
            rec(tail, updatedPlayer)
        }
      }
      rec(rolls, this)
    }
  }

  def part1(file: String): Int = {
    val players = fileReader(file).map { l =>
      val pattern = "Player ([0-9]+) starting position: ([0-9]+)".r
      val pattern(number, position) = l
      Player(number.toInt, position.toInt, 0, 0)
    }
    playGame(players)
  }

  private def playGame(players: Seq[Player]): Int = {
    @tailrec
    def rec(players: Seq[Player]): Int = {
      players match {
        case head :: tail =>
          val totalRolls = players.map(_.turns).sum * 3

          def roll(offset: Int): Int = {
            (totalRolls + offset) % 100 match {
              case 0 => 100
              case r => r
            }
          }

          val rolls = Seq(roll(1), roll(2), roll(3))
          val updatedPlayer = head.takeTurn(rolls)
          if (updatedPlayer.points >= 1000) {
            val loserPoints = tail.map(_.points).sum
            val rolls = (tail.map(_.turns).sum * 3) + (updatedPlayer.turns * 3)
            loserPoints * rolls
          } else {
            rec(tail :+ updatedPlayer)
          }
      }
    }

    rec(players)
  }

}
