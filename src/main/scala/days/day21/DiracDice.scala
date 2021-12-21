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
    def takeTurn(dice: Dice): (Player, Dice) = {
      @tailrec
      def rec(player: Player, dice: Dice, rolls: Int = 0): (Player, Dice) = {
        if (rolls >= 3) {
          (player.copy(points = points + player.position, turns = turns + 1), dice)
        } else {
          val rolledDice = dice.roll
          val updatedPosition = (player.position + rolledDice.score) % 10 match {
            case 0 => 10
            case p => p
          }
          rec(player.copy(position = updatedPosition), rolledDice, rolls + 1)
        }
      }
      rec(this, dice)
    }
  }

  case class Dice(score: Int) {
    def roll: Dice = this.copy(score = score + 1)
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
    def rec(players: Seq[Player], dice: Dice = Dice(0)): Int = {
      players match {
        case head :: tail =>
          val (updatedPlayer, updatedDice) = head.takeTurn(dice)
          if (updatedPlayer.points >= 1000) {
            val loserPoints = tail.map(_.points).sum
            val rolls = (tail.map(_.turns).sum * 3) + (updatedPlayer.turns * 3)
            loserPoints * rolls
          } else {
            rec(tail :+ updatedPlayer, updatedDice)
          }
      }
    }

    rec(players)
  }

}
