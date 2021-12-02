case class Submarine(horizontalPosition: Int, depth: Int, aim: Int) {

  def applyInstruction(instruction: String): Submarine = {
    instruction match {
      case s"forward $x" => this.copy(horizontalPosition = horizontalPosition + x.toInt, depth = depth + (x.toInt * aim))
      case s"down $x" => this.copy(aim = aim + x.toInt)
      case s"up $x" => this.copy(aim = aim - x.toInt)
      case _ => this
    }
  }

  def multiplyPositions: Int = horizontalPosition * depth
}

object Submarine {
  def apply(): Submarine = new Submarine(0, 0, 0)

  def applyInstructions(instructions: Seq[String]): Submarine = instructions.foldLeft(Submarine())((submarine, instruction) => {
    submarine.applyInstruction(instruction)
  })
}
