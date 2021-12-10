import scala.collection.mutable
import scala.io.Source

object D10 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromFile("input/d10.txt").getLines().toList
    one(input)
    two(input)
  }

  def one(input: Seq[String]) = {
    val sum = input
      .map(findIllegalInstruction)
      .filter(_.isDefined)
      .map(illegalInstruction => calculateIllegalCharScore(illegalInstruction.get))
      .sum

    println(sum)
  }

  def two(input: Seq[String]) = {
    val sorted = input
      .filter(instruction => findIllegalInstruction(instruction).isEmpty)
      .map(closeUncompletedInstruction)
      .map(paddedInstructions => {
        var tmpSum: BigInt = 0
        val scores = paddedInstructions.map {
          case ")" => 1
          case "]" => 2
          case "}" => 3
          case ">" => 4
        }
        scores.foreach { score =>
          tmpSum *= 5
          tmpSum += score
        }
        tmpSum
      })
      .sorted
    println(sorted(sorted.length / 2))
  }

  def findIllegalInstruction(instruction: String): Option[String] = {
    val openChunks: mutable.Stack[String] = new mutable.Stack[String]()

    for (i <- 0 until instruction.length) {
      val curr = instruction.charAt(i).toString

      if (isCloseInstruction(curr)) {
        if (openChunks.isEmpty) {
          println(s"found illegal char $curr in $instruction")
          return Some(curr)
        }

        val openChuck = openChunks.pop()
        if (closingStr(openChuck) != curr) {
          println(s"found illegal char $curr in $instruction")
          return Some(curr)
        }
      } else {
        openChunks.push(curr)
      }
    }
    None
  }

  def closeUncompletedInstruction(instruction: String): List[String] = {
    val openChunks: mutable.Stack[String] = new mutable.Stack[String]()

    for (i <- 0 until instruction.length) {
      val curr = instruction.charAt(i).toString
      if (isCloseInstruction(curr)) {
        openChunks.pop()
      } else {
        openChunks.push(curr)
      }
    }

    openChunks.map(closingStr).toList
  }

  def isCloseInstruction(instruction: String): Boolean = {
    instruction match {
      case "}" => true
      case ")" => true
      case ">" => true
      case "]" => true
      case _ => false
    }
  }

  def closingStr(s: String): String = {
    s match {
      case "<" => ">"
      case "(" => ")"
      case "[" => "]"
      case "{" => "}"
    }
  }

  def calculateIllegalCharScore(illegalChar: String): Int = {
    illegalChar match {
      case ")" => 3
      case "]" => 57
      case "}" => 1197
      case ">" => 25137
      case c@_ => throw new IllegalArgumentException(s"wrong char $c")
    }
  }
}
