package puzzle6

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Solution {
  val input: String = Source.fromResource("input6.txt")
    .getLines
    .toSeq
    .head
  
  def partOne: Int = {
    input.sliding(4).zipWithIndex.collectFirst {
      case (str, idx) if str.toSet.size == str.length => idx + 4
    }.getOrElse(throw new Exception("OK WHAT BROKE???"))
  }
  
  def partTwo: Int = {
    input.sliding(14).zipWithIndex.collectFirst {
      case (str, idx) if str.toSet.size == str.length => idx + 14
    }.getOrElse(throw new Exception("OK WHAT BROKE???"))
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}