package days.day12

import utils.FileReader.fileReader

object PassagePathing {

  type Path = Seq[Cave]
  type Paths = Set[Path]

  case class Cave(name: String) {
    def isSmall: Boolean = name.toLowerCase == name
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
    println()
  }

  def part1(file: String): Int = {
    val input = fileReader(file).map(CaveConnection(_))
    findPaths(input).size
  }

  def findPaths(connections: Seq[CaveConnection]): Paths = {

    def rec(currentCave: Cave, cavesToAvoid: Seq[Cave] = Nil, path: Path = Nil): Paths = {
      val possibleConnections = connections
        .filter(_.contains(currentCave))
        .filter(c => !cavesToAvoid.contains(c.otherCave(currentCave)))

      possibleConnections.flatMap { pc =>
        val nextCave = pc.otherCave(currentCave)
        val updatedPath = path :+ currentCave
        if (currentCave == Cave("end")) {
          updatedPath :: Nil
        } else {
          val updatedCavesToAvoid = if (currentCave.isSmall) cavesToAvoid :+ currentCave else cavesToAvoid
          rec(nextCave, updatedCavesToAvoid, updatedPath)
        }
      }.toSet
    }

    rec(Cave("start"))
  }

}
