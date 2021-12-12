package days.day12

import utils.FileReader.fileReader

object PassagePathing {

  type Path = Seq[Cave]
  type Paths = Set[Path]

  final val START = "start"
  final val END = "end"

  case class Cave(name: String) {
    def isSmall: Boolean = name.toLowerCase == name
    def isStart: Boolean = name == START
    def isEnd: Boolean = name == END
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

  def part1(file: String): Int = run(file, canVisitASmallCaveTwice = false)

  def part2(file: String): Int = run(file, canVisitASmallCaveTwice = true)

  def run(file: String, canVisitASmallCaveTwice: Boolean): Int = {
    val input = fileReader(file).map(CaveConnection(_))
    findPaths(input, canVisitASmallCaveTwice).size
  }

  def findPaths(connections: Seq[CaveConnection], canVisitASmallCaveTwice: Boolean): Paths = {

    def rec(currentCave: Cave, visitedCaves: Map[Cave, Int] = Map(), visitedASmallCaveTwice: Boolean = false, path: Path = Nil): Paths = {
      val possibleConnections = connections
        .filter(_.contains(currentCave))
        .filterNot { c =>
          val otherCave = c.otherCave(currentCave)
          val otherCaveCount = visitedCaves.getOrElse(otherCave, 0)
          val otherCaveAllowedCount = if (canVisitASmallCaveTwice && !visitedASmallCaveTwice) 2 else 1
          otherCave.isSmall && otherCaveCount >= otherCaveAllowedCount
        }

      possibleConnections.flatMap { pc =>
        val nextCave = pc.otherCave(currentCave)
        val updatedPath = path :+ currentCave
        if (currentCave.isEnd) {
          updatedPath :: Nil
        } else if (nextCave.isStart) {
          Nil
        } else {
          rec(
            currentCave = nextCave,
            visitedCaves = combineMaps(visitedCaves, Map(currentCave -> 1)),
            visitedASmallCaveTwice = if (nextCave.isSmall && visitedCaves.contains(nextCave)) true else visitedASmallCaveTwice,
            path = updatedPath
          )
        }
      }.toSet
    }

    rec(Cave(START))
  }

  private def combineMaps(a: Map[Cave, Int], b: Map[Cave, Int]): Map[Cave, Int] =
    a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0)) }

}
