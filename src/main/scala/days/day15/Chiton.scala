package days.day15

import utils.FileReader.fileReader

import scala.annotation.tailrec

object Chiton {

  case class Node(x: Int, y: Int, value: Int) {
    def isRoot: Boolean = x == 0 && y == 0
    val risk: Int = if (isRoot) 0 else value
  }

  type Matrix = Array[Array[Node]]

  def run(): Unit = {
    val file = "/day-15-input.txt"
    println(s"Day 15 part 1 result: ${part1(file)}")
    println()
  }

  def part1(file: String): Int = {
    val matrix = initialiseMatrix(file)
    val result = findPaths(matrix)

    result(matrix.length - 1)(matrix.head.length - 1).risk
  }

  private def findPaths(matrix: Matrix): Matrix = {

    @tailrec
    def rec(queue: Seq[Node], risks: Matrix, visitedNodes: Seq[Node] = Nil): Matrix = {
      queue match {
        case Nil => risks
        case _ =>

          def adjacentNodes(node: Node): Seq[Node] = {
            var adjacentNodes: Seq[Node] = Nil
            for (dx <- -1 to 1; dy <- -1 to 1) {
              if (dx == 0 ^ dy == 0) {
                try adjacentNodes = adjacentNodes :+ matrix(node.y + dy)(node.x + dx)
                catch {
                  case _: ArrayIndexOutOfBoundsException => ()
                }
              }
            }
            adjacentNodes
          }

          val node = risks
            .flatten
            .filterNot(visitedNodes.contains(_))
            .minBy(_.risk)

          adjacentNodes(node).foreach { adjNode =>
            risks(adjNode.y)(adjNode.x) = Node(
              adjNode.x,
              adjNode.y,
              Math.min(
                risks(node.y)(node.x).risk + adjNode.risk,
                risks(adjNode.y)(adjNode.x).risk
              )
            )
          }

          rec(
            queue = queue.filterNot(n => n.x == node.x && n.y == node.y),
            risks = risks,
            visitedNodes = visitedNodes :+ node
          )
      }
    }

    val risks = matrix.map(_.map(n => n.copy(value = if (n.isRoot) 0 else Int.MaxValue)))
    rec(matrix.flatten.toSeq, risks)
  }

  private def initialiseMatrix(file: String): Matrix =
    fileReader(file)
      .zipWithIndex
      .map(y => y._1.zipWithIndex.map(x => Node(x._2, y._2, x._1.toString.toInt)).toArray)
      .toArray

}
