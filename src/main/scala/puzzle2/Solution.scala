package puzzle2

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input2.txt")
    .getLines
    .toSeq

  val playerMap = Map(
    "X" -> 1,
    "Y" -> 2,
    "Z" -> 3
  )
  
  val winLossMap = Map(
    "A X" -> 3,
    "A Y" -> 6,
    "A Z" -> 0,
    "B X" -> 0,
    "B Y" -> 3,
    "B Z" -> 6,
    "C X" -> 6,
    "C Y" -> 0,
    "C Z" -> 3
  )
  
  def partOne: Int = {
    input.map { pair =>
      val outcome = winLossMap(pair)
      val play = pair.split(" ").toSeq.tail.head
      val playScore = playerMap(play)
      outcome + playScore
    }.sum
  }
  
  def outcomeMap = Map(
    "X" -> 0,
    "Y" -> 3,
    "Z" -> 6
  )
  
  def partTwo: Int = {
    input.map { pair =>
      val opponentPlay = pair.split(" ").toSeq.head
      val outcome = pair.split(" ").toSeq.tail.head
      val outcomeScore = outcomeMap(outcome)
      val play = winLossMap.collect {
        case (pair, score) if pair.startsWith(opponentPlay) && score == outcomeScore => pair.split(" ").toSeq.tail.head
      }.head
      val playScore = playerMap(play)
      outcomeScore + playScore
    }.sum
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}