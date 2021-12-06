package days.day4

case class BingoBoard(numbers: Seq[Seq[BingoNumber]]) {

  def updateBoard(drawnNumber: Int): BingoBoard =
    this.copy(numbers = numbers.map(_.map(_.update(drawnNumber))))

  def isComplete: Boolean = {
    def completeness(numbers: Seq[BingoNumber]): Boolean = numbers.forall(_.hasBeenDrawn)
    def hasCompleteRow: Boolean = numbers.exists(completeness)
    def hasCompleteColumn: Boolean = numbers.transpose.exists(completeness)
    hasCompleteRow || hasCompleteColumn
  }

  def sumOfUnmarkedNumbers: Int = numbers.flatten.filter(!_.hasBeenDrawn).map(_.number).sum
}

object BingoBoard {
  def initialise(lines: Seq[String]): BingoBoard = {
    val numbers = lines.map(_
      .split(" ")
      .filterNot(_.trim.isEmpty)
      .map(x => BingoNumber(x.toInt))
      .toSeq
    )

    BingoBoard(numbers)
  }
}
