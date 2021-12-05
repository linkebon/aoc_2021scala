import scala.io.Source

object D5 {
  def main(args: Array[String]): Unit = {
    Source.fromFile("input/d5.txt")
    one()
  }

  def one(): Unit = {
    val input: List[((Int, Int), (Int, Int))] = Source.fromFile("input/d5.txt")
      .getLines()
      .map { s =>
        val splitted = s.split("->")
        val x1 :: y1 :: Nil = splitted.head.split(",").map(_.trim).toList
        val x2 :: y2 :: Nil = splitted.tail.head.split(",").map(_.trim).toList
        ((x1.toInt -> y1.toInt) -> (x2.toInt -> y2.toInt))
      }.toList

    val (xMax, yMax) = findMatrixDimension(input)

    val matrix = Array.ofDim[Int](xMax, yMax)

    input.foreach {
      // vertical
      case ((x1, y1), (x2, y2)) if x1 == x2 =>
        val increasing = y1 < y2
        for (r <- if (increasing) y1 to y2 else y2 to y1) {
          matrix(x1)(r) += 1
        }

      // horizontel
      case ((x1, y1), (x2, y2)) if y1 == y2 =>
        val increasing = x1 < x2
        for (r <- if (increasing) x1 to x2 else x2 to x1) {
          matrix(r)(y1) += 1
        }

      case _ =>
    }

    val atleastTwoOverlapSum = (for {
      x <- matrix
      y <- x
      if y > 1
    } yield y).length


    println(atleastTwoOverlapSum)
  }


  def findMatrixDimension(coordinates: List[((Int, Int), (Int, Int))]): (Int, Int) = {
    var xMax = 0
    var yMax = 0

    coordinates.foreach { points =>
      if (points._1._1 > xMax)
        xMax = points._1._1

      if (points._1._2 > yMax)
        yMax = points._1._2

      if (points._2._1 > xMax)
        xMax = points._2._1

      if (points._2._2 > yMax)
        yMax = points._2._2
    }
    (xMax + 1, yMax + 1)
  }


  case class Point(var overlapped: Int = 0)

}
