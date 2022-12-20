package puzzle3

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input3.txt")
    .getLines
    .toSeq
  
  val lower =  (1 to 26).map(n => (n + 96).toChar).toSet
  val upper =  (1 to 26).map(n => (n + 38).toChar).toSet
  
  def partOne: Int = {
    input.map { str =>
      val (left, right) = str.splitAt(str.length / 2)
      val c = left.toCharArray.toSet.intersect(right.toCharArray.toSet).head
      if (lower.contains(c)) c.toInt - 96
      else c.toInt - 38
    }.sum
  }
  
  def partTwo: Int = {
    input.sliding(3, 3).map { lines =>
      val intersection = lines.tail.foldLeft(lines.head.toCharArray.toSet) {
        case (intersection, next) => intersection.intersect(next.toCharArray.toSet)
      }
      val c = intersection.head
      val num = if (lower.contains(c)) c.toInt - 96
      else c.toInt - 38
      num
    }.sum
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}