import scala.io.Source
import scala.util.control.Breaks
import scala.util.control.Breaks.break

object D4 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromFile("input/d4.txt").mkString.split("\n\n").toList
    val bingoNumbers: List[String] = input.head.split(",").toList;
    val bingoBoards: Seq[Array[Array[Point]]] = input.tail.map(parseBingoBoard)

    partOne(bingoNumbers, bingoBoards)

  }

  def partOne(bingoNumbers: List[String], bingoBoards: Seq[Array[Array[Point]]]): Unit = {
    val loop = new Breaks
    loop.breakable {
      for (number <- bingoNumbers) {
        markNumberInBingoBoards(number, bingoBoards)
        val (bingoBoard, bingo) = isBingo(bingoBoards)
        if (bingo) {
          println(calcUnmarkedNumbersSum(bingoBoard.get) * number.toInt)
          loop.break()
        }
      }
    }
  }

  def isBingo(bingoBoards: Seq[Array[Array[Point]]]): (Option[Array[Array[Point]]], Boolean) = {
    for (board <- bingoBoards) {
      for (row <- board) {
        if (allNumbersInRowMarked(row)) {
          return (Some(board), true)
        }
      }

      for (colIdx <- 0 until 4) {
        if (board(colIdx)(0).chosen && board(colIdx)(1).chosen && board(colIdx)(2).chosen && board(colIdx)(3).chosen && board(colIdx)(4).chosen) {
          return (Some(board), true)
        }
      }
    }
    (None, false)
  }

  def allNumbersInRowMarked(row: Array[Point]): Boolean = {
    row.forall(_.chosen)
  }

  def calcUnmarkedNumbersSum(board: Array[Array[Point]]): Int = {
    var sum = 0;
    for (row <- board; point <- row) {
      if (!point.chosen) {
        sum += point.value.toInt
      }
    }
    sum
  }

  def markNumberInBingoBoards(number: String, bingoBoards: Seq[Array[Array[Point]]]): Unit = {
    for (board <- bingoBoards; row <- board; point <- row) {
      if (point.value == number) {
        point.chosen = true
      }
    }
  }

  def parseBingoBoard(board: String): Array[Array[Point]] = {
    val bingoBoard: Array[Array[Point]] =
      board.replace("  ", " ")
        .split("\n")
        .map(s => s.trim.split(" "))
        .map(line => line.map(l => Point(l)))
    bingoBoard
  }

  case class Point(value: String, var chosen: Boolean = false)

}


