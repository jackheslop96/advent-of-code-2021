package fish

import scala.annotation.tailrec

case class LanternFish(daysUntilNewFish: Int, newFishNeedsCreating: Boolean = false) {

  def simulateDay: LanternFish = daysUntilNewFish match {
    case 0 => this.copy(daysUntilNewFish = 6, newFishNeedsCreating = true)
    case x => this.copy(daysUntilNewFish = x - 1)
  }

  def resetFlag: LanternFish = this.copy(newFishNeedsCreating = false)
}

object LanternFish {

  def modelPopulationGrowth(input: Seq[String], days: Int): Int = {
    val fish = getListOfFish(input.head)
    simulateNDays(fish, days).size
  }

  def apply(daysUntilNewFish: Int) = new LanternFish(daysUntilNewFish)

  def getListOfFish(input: String): Seq[LanternFish] = input
    .split(",")
    .map(_.toInt)
    .map(LanternFish(_))
    .toSeq

  def simulateNDays(lanternFish: Seq[LanternFish], n: Int): Seq[LanternFish] = {
    @tailrec
    def rec(lanternFish: Seq[LanternFish], n: Int): Seq[LanternFish] = n match {
      case 0 => lanternFish
      case _ => rec(simulateDay(lanternFish), n - 1)
    }
    rec(lanternFish, n)
  }

  def simulateDay(lanternFish: Seq[LanternFish]): Seq[LanternFish] =
    appendFish(lanternFish.map(_.simulateDay))

  private def appendFish(lanternFish: Seq[LanternFish]): Seq[LanternFish] = {
    val numberOfNewFish = lanternFish.count(_.newFishNeedsCreating)
    lanternFish.map(_.resetFlag) ++ Seq.fill(numberOfNewFish)(LanternFish(8))
  }
}
