package days.day21

import utils.FileReader.fileReader

import scala.annotation.tailrec

object DiracDice {

  def run(): Unit = {
    val file = "/day-21-input.txt"
    println(s"Day 21 part 1 result: ${part1(file)}")
    println(s"Day 21 part 2 result: ${part2(file)}")
    println()
  }

  type Players = (Player, Player)

  case class Player(position: Int, points: Int) {
    def takeTurn(rolls: Seq[Int]): Player = {
      @tailrec
      def rec(rolls: Seq[Int], player: Player): Player = {
        rolls match {
          case Nil => player.copy(points = points + player.position)
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
    @tailrec
    def rec(players: Players, turns: Int = 0): Int = {
      players match {
        case (currentPlayer, otherPlayer) =>
          val totalRolls = 3 * turns

          def roll(offset: Int): Int =
            (totalRolls + offset) % 100 match {
              case 0 => 100
              case r => r
            }

          val rolls = Seq(roll(1), roll(2), roll(3))
          val currentPlayerUpdated = currentPlayer.takeTurn(rolls)
          if (currentPlayerUpdated.points >= 1000) {
            val loserPoints = otherPlayer.points
            loserPoints * (totalRolls + 3)
          } else {
            rec((otherPlayer, currentPlayerUpdated), turns + 1)
          }
      }
    }
    rec(initialisePlayers(file))
  }

  def part2(file: String): Long = {
    val dice = for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield Seq(a, b, c)
    @tailrec
    def rec(states: Map[Players, Long], winCounts: (Long, Long) = (0L, 0L)): Long = {
      val newStates = states.foldLeft(Map[Players, Long]().withDefaultValue(0L)) {
        case (newStates, ((currentPlayer, otherPlayer), currentStateCount)) =>
          dice.foldLeft(newStates) { (newStates, rolls) =>
            val newPlayers = (otherPlayer, currentPlayer.takeTurn(rolls))
            newStates.updated(newPlayers, newStates(newPlayers) + currentStateCount)
          }
      }
      val (wonStates, stillPlaying) = newStates.partition {
        case ((_, currentPlayer), _) => currentPlayer.points >= 21
      }
      val newWinCount = (winCounts._2, winCounts._1 + wonStates.values.sum)
      if (stillPlaying.isEmpty) {
        Math.max(newWinCount._1, newWinCount._2)
      } else {
        rec(stillPlaying, newWinCount)
      }
    }
    rec(Map(initialisePlayers(file) -> 1L))
  }

  private def initialisePlayers(file: String): Players = {
    def player(line: String): Player = {
      val pattern = "Player ([0-9]+) starting position: ([0-9]+)".r
      val pattern(_, position) = line
      Player(position.toInt, 0)
    }
    val lines = fileReader(file)
    assert(lines.length == 2)
    (player(lines.head), player(lines.last))
  }

}
