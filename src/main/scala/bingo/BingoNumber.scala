package bingo

case class BingoNumber(number: Int, hasBeenDrawn: Boolean = false) {

  def update(drawnNumber: Int): BingoNumber = this.copy(hasBeenDrawn = hasBeenDrawn || number == drawnNumber)
}
