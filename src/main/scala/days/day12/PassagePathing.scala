package days.day12

import utils.FileReader.fileReader

object PassagePathing {

  type Path = Seq[Cave]
  type Paths = Set[Path]

  case class Cave(name: String) {
    def isSmall: Boolean = name.toLowerCase == name
    def isStart: Boolean = name == "start"
  }
  case class CaveConnection(from: Cave, to: Cave) {
    def contains(cave: Cave): Boolean = this.from == cave || this.to == cave

    def otherCave(that: Cave): Cave = if (that == from) to else from
  }
  object CaveConnection {
    def apply(line: String): CaveConnection = {
      val names = line.split("-")
      new CaveConnection(Cave(names.head), Cave(names.last))
    }
  }

  def run(): Unit = {
    val file = "/day-12-input.txt"
    println(s"Day 12 part 1 result: ${part1(file)}")
    println(s"Day 12 part 2 result: ${part2(file)}")
    println()
  }

  def part1(file: String): Int = {
    val input = fileReader(file).map(CaveConnection(_))
    findPaths(input).size
  }

  def part2(file: String): Int = {
    val input = fileReader(file).map(CaveConnection(_))
    findPaths2(input).size
  }

  def findPaths(connections: Seq[CaveConnection]): Paths = {

    def rec(currentCave: Cave, visitedCaves: Map[Cave, Int] = Map(), path: Path = Nil): Paths = {
      val possibleConnections = connections
        .filter(_.contains(currentCave))
        .filter { c =>
          val otherCave = c.otherCave(currentCave)
          val otherCaveCount = visitedCaves.getOrElse(otherCave, 0)
          !(otherCave.isSmall && otherCaveCount >= 1)
        }

      possibleConnections.flatMap { pc =>
        val nextCave = pc.otherCave(currentCave)
        val updatedPath = path :+ currentCave
        if (currentCave == Cave("end")) {
          updatedPath :: Nil
        } else {
          val updatedVisitedCaves = combineMaps(visitedCaves, Map(currentCave -> 1))
          rec(nextCave, updatedVisitedCaves, updatedPath)
        }
      }.toSet
    }

    rec(Cave("start"))
  }

  def findPaths2(connections: Seq[CaveConnection]): Paths = {

    def rec(currentCave: Cave, visitedCaves: Map[Cave, Int] = Map(), visitedASmallCaveTwice: Boolean = false, path: Path = Nil): Paths = {
      val possibleConnections = connections
        .filter(_.contains(currentCave))
        .filter { c =>
          val otherCave = c.otherCave(currentCave)
          val otherCaveCount = visitedCaves.getOrElse(otherCave, 0)
          if (visitedASmallCaveTwice) {
            !(otherCave.isSmall && otherCaveCount >= 1)
          } else {
            !(otherCave.isSmall && otherCaveCount >= 2)
          }
        }

      possibleConnections.flatMap { pc =>
        val nextCave = pc.otherCave(currentCave)
        val updatedPath = path :+ currentCave
        if (currentCave == Cave("end")) {
          updatedPath :: Nil
        } else {
          if (nextCave.isStart) {
            Nil
          } else {
            val updatedVisitedCaves = combineMaps(visitedCaves, Map(currentCave -> 1))
            val visitedNextCaveBefore = visitedCaves.getOrElse(nextCave, 0) > 0
            if (nextCave.isSmall && visitedNextCaveBefore) {
              rec(nextCave, updatedVisitedCaves, visitedASmallCaveTwice = true, updatedPath)
            } else {
              rec(nextCave, updatedVisitedCaves, visitedASmallCaveTwice, updatedPath)
            }
          }
        }
      }.toSet
    }

    rec(Cave("start"))
  }

  private def combineMaps(a: Map[Cave, Int], b: Map[Cave, Int]): Map[Cave, Int] = {
    a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0)) }
  }

}
