import java.util.stream.IntStream

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object D9 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input/d9.txt")
      .mkString
      .split("\n")
      .toList

    val heights: Seq[List[Int]] = structureInput(input)
    one(heights)
  }

  def one(heights: Seq[List[Int]]): Unit = {
    val xMax = heights.length
    val yMax = heights.head.length

    val lowPointLocations = ListBuffer.empty[(Int, Int)]

    for (x <- 0 until xMax; y <- 0 until yMax) {
      if (isLowPointHeight(x -> y, heights, xMax, yMax))
        lowPointLocations.append(x -> y)
    }
    println(lowPointLocations.map(c => heights(c._1)(c._2) + 1).sum)
  }

  def isLowPointHeight(c: (Int, Int), heights: Seq[List[Int]], xMax: Int, yMax: Int): Boolean = {
    c match {
      case x -> y =>
        val v = heights(x)(y)
        (Try(v < heights(x - 1)(y)).getOrElse(true)
          && Try(v < heights(x)(y - 1)).getOrElse(true)
          && Try(v < heights(x)(y + 1)).getOrElse(true)
          && Try(v < heights(x + 1)(y)).getOrElse(true))
    }
  }

  def structureInput(input: Seq[String]) = {
    val heights = ListBuffer.empty[List[Int]]
    for (str <- input) {
      val temp = ListBuffer.empty[Int]
      for (i <- 0 until str.length) {
        temp.append(str.charAt(i).toString.toInt)
      }
      heights.append(temp.toList)
    }
    heights.toList
  }

}
