import scala.collection.mutable.ListBuffer
import scala.io.Source

object D6 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input/d6.txt")
      .mkString
      .split(",")
      .map(_.toInt)
      .toList

    one(input)

  }

  def one(input: List[Int]): Unit = {
    val t1 = System.currentTimeMillis()
    val lanternFish = readInitialState(input)
    for(_ <- 1 to 80) {
      lanternFish.runCycle()
    }
    println(s"${System.currentTimeMillis() - t1} ms. fish count: " + lanternFish.countFishes())
  }

  def printFishes(day: Int, lanternFish: LanternFish): Unit = {
    print(s"Day $day: ")
    lanternFish.fishPrint()
    println()
  }

  def readInitialState(input: List[Int]): LanternFish = {
    val reversedInput = input.reverse
    val first = LanternFish(reversedInput.head)
    reversedInput.tail.foldLeft(first) { (acc: LanternFish, newFishCycle: Int) =>
      LanternFish(newFishCycle, ListBuffer(acc))
    }
  }

  case class LanternFish(var cycle: Int = 0, var children: ListBuffer[LanternFish] = ListBuffer.empty) {
    def runCycle(): Unit = {
      children.foreach(_.runCycle())
      if (cycle == 0) {
        addFish(8)
        cycle = 6
      } else {
        cycle -= 1
      }
    }

    def addFish(newCycle: Int): Unit = {
      children.append(LanternFish(newCycle))
    }

    def fishPrint(): Unit = {
      val suffix = if (children.isEmpty) "" else ","
      //print(s"$cycle,")
      children.foreach(_.fishPrint())
    }

    def countFishes(): Int = {
      1 + children.map(_.countFishes()).sum
    }
  }


}
