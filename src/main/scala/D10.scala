import scala.collection.mutable
import scala.io.Source

object D10 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromFile("input/d10.txt").getLines().toList
    val completeInstructions = input
    val sum = completeInstructions
      .map(findIllegalInstruction)
      .filter(_.isDefined)
      .map(illegalInstruction => calculateIllegalCharScore(illegalInstruction.get))
      .sum

    println(sum)
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

  def calculateIllegalCharScore(illegarChar: String): Int = {
    illegarChar match {
      case ")" => 3
      case "]" => 57
      case "}" => 1197
      case ">" => 25137
      case c@_ => throw new IllegalArgumentException(s"wrong char $c")
    }
  }
}
