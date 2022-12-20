package puzzle4

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input4.txt")
    .getLines
    .toSeq
  
  def partOne: Int = {
    input.count {
      str =>
        str.split(",").toSeq.map {
          _.split("-").toSeq match {
            case Seq(a, b) => (a.toInt to b.toInt).toSet
          }
        } match {
          case Seq(s1, s2) => 
            val intersection = s1.intersect(s2)
            intersection == s1 || intersection == s2
        }
    }
  }
  
  def partTwo: Int = {
    input.count {
      str =>
        str.split(",").toSeq.map {
          _.split("-").toSeq match {
            case Seq(a, b) => (a.toInt to b.toInt).toSet
          }
        } match {
          case Seq(s1, s2) =>
            s1.intersect(s2).nonEmpty
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}