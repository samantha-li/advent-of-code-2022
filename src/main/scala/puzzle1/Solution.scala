package puzzle1

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input1.txt")
    .getLines
    .toSeq

  def partOne: Int = {
    input.mkString(",").split(",,").map { str =>
      println(str)
      str.split(",").filter(_.nonEmpty).map(_.toInt).sum
    }.max
  }

  def partTwo: Int = {
    input.mkString(",").split(",,").map { str =>
      println(str)
      str.split(",").filter(_.nonEmpty).map(_.toInt).sum
    }.sorted.reverse.take(3).sum
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}