package days.day15

import utils.FileReader.fileReader

import scala.collection.mutable

object Chiton {

  case class Node(x: Int, y: Int)

  type Grid = Map[Node, Int]

  def run(): Unit = {
    val file = "/day-15-input.txt"
    println(s"Day 15 part 1 result: ${part1(file)}")
    println(s"Day 15 part 2 result: ${part2(file)}")
    println()
  }

  def part1(file: String): Int = {
    val grid = initialiseGrid(file)
    findLowestRiskPath(grid)
  }

  def part2(file: String): Int = {
    val grid = initialiseGrid(file)
    findLowestRiskPath(expandGrid(grid))
  }

  // uses Dijkstra's algorithm
  def findLowestRiskPath(grid: Grid): Int = {
    val startNode = Node(0, 0)
    val risks = mutable.Map(startNode -> 0)
    val queue = mutable.Queue(startNode)

    def adjacentNodes(node: Node): Seq[Node] = {
      val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))
      directions
        .map {
          case (dx, dy) => Node(node.x + dx, node.y + dy)
        }
        .filter(grid.contains)
    }

    do {
      val node = queue.dequeue()
      adjacentNodes(node).foreach { adjacentNode =>
        if (!risks.contains(adjacentNode) || (risks(node) + grid(adjacentNode) < risks(adjacentNode))) {
          risks(adjacentNode) = risks(node) + grid(adjacentNode)
          queue.enqueue(adjacentNode)
        }
      }
    }
    while (queue.nonEmpty)

    risks(findEndNode(grid))
  }

  private def initialiseGrid(file: String): Grid = {
    val input = fileReader(file)
    Seq.tabulate(input.head.length, input.length) { (x, y) =>
      Node(x, y) -> input(y)(x).toString.toInt
    }.flatten.toMap
  }

  private def expandGrid(grid: Grid): Grid = {
    val endNode = findEndNode(grid)
    val gridWidth = endNode.x + 1
    val gridLength = endNode.y + 1
    Seq.tabulate(5, 5) { (x, y) =>
      grid.map { case (node, risk) =>
        val newRisk = risk + x + y match {
          case r if r > 9 => r % 9
          case r => r
        }
        Node((gridWidth * x) + node.x, (gridLength * y) + node.y) -> newRisk
      }
    }.flatten.flatten.toMap
  }

  private def findEndNode(grid: Grid): Node = grid.keys.maxBy(p => p.x + p.y)

}
