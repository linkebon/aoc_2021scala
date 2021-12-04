import scala.collection.{mutable}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks

object D4 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromFile("input/d4.txt").mkString.split("\n\n").toList
    val bingoNumbers: List[String] = input.head.split(",").toList
    val bingoBoards: Seq[Array[Array[Point]]] = input.tail.map(parseBingoBoard)
    val bingoBoardsPartTwo: Seq[Array[Array[Point]]] = input.tail.map(parseBingoBoard)

    partOne(bingoNumbers, ListBuffer.empty ++= bingoBoards)
    partTwo(bingoNumbers, ListBuffer.empty ++= bingoBoardsPartTwo)
  }

  def partTwo(bingoNumbers: List[String], bingoBoards: ListBuffer[Array[Array[Point]]]): Unit = {
    var lastBingoBoard: Array[Array[Point]] = Array.empty
    var lastBingoNumber = ""
    val loop = new Breaks
    loop.breakable {
      for (number <- bingoNumbers) {
        markNumberInBingoBoards(number, bingoBoards)
        val boardsWithBingo = isBingoSupportingMultipleBingo(bingoBoards)
        if (boardsWithBingo.isDefined) {
          boardsWithBingo.get.foreach(b => {
            bingoBoards.remove(b._2)
            lastBingoNumber = number
            lastBingoBoard = b._1
          }
          )
        }
      }
    }
    println("part two: " + calcUnmarkedNumbersSum(lastBingoBoard) * lastBingoNumber.toInt)

  }

  def partOne(bingoNumbers: List[String], bingoBoards: mutable.Seq[Array[Array[Point]]]): Unit = {
    val loop = new Breaks
    loop.breakable {
      for (number <- bingoNumbers) {
        markNumberInBingoBoards(number, bingoBoards)
        val (bingoBoard, idx) = isBingo(bingoBoards)
        if (bingoBoard.isDefined) {
          println("part one: " + calcUnmarkedNumbersSum(bingoBoard.get) * number.toInt)
          loop.break()
        }
      }
    }
  }

  def isBingo(bingoBoards: mutable.Seq[Array[Array[Point]]]): (Option[Array[Array[Point]]], Int) = {

    for ((board, idx) <- bingoBoards.zipWithIndex) {
      for (row <- board) {
        // check rows
        if (row.forall(_.chosen)) {
          return (Some(board), idx)
        }
      }

      // check columns by using transpose so columns gets as rows
      for (col <- board.transpose) {
        if (col.forall(_.chosen)) {
          return (Some(board), idx)
        }
      }
    }
    (None, -1)
  }

  def isBingoSupportingMultipleBingo(bingoBoards: mutable.Seq[Array[Array[Point]]]): Option[ListBuffer[(Array[Array[Point]], Int)]] = {
    var boardsGotBingo: ListBuffer[Tuple2[Array[Array[Point]], Int]] = ListBuffer.empty
    for ((board, idx) <- bingoBoards.zipWithIndex) {
      var gotBingo = false
      for (row <- board) {
        // check rows
        if (row.forall(_.chosen)) {
          boardsGotBingo.prepend((board, idx))
          gotBingo = true
        }
      }
      if (!gotBingo) {
        // check columns by using transpose so columns gets as rows
        for (col <- board.transpose) {
          if (col.forall(_.chosen)) {
            boardsGotBingo.prepend((board, idx))
          }
        }
      }
    }

    if (boardsGotBingo.nonEmpty)
      Some(boardsGotBingo)
    else
      None
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

  def markNumberInBingoBoards(number: String, bingoBoards: mutable.Seq[Array[Array[Point]]]): Unit = {
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


