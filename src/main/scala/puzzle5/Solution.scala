package puzzle5

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Solution {
  val input: Seq[String] = Source.fromResource("input5.txt")
    .getLines
    .toSeq
  
  case class Move(n: Int, src: Int, dest: Int) {
    def rendered: String = s"Move $n from $src to $dest"
  }
  val regex = "move ([0-9]*) from ([0-9]*) to ([0-9]*)".r
  
  def parseCrates(m: Seq[String]): Array[Seq[Char]] = {
    val numColumns = m.last.sliding(4, 4).toSeq.last.strip().toInt
    println(s"numColumns: $numColumns")
    m.dropRight(1).foldLeft((0 until numColumns).map(_ => Seq.empty[Char]).toArray){
      case (arr, row) => row.sliding(4, 4).zipWithIndex.foldLeft(arr) {
        case (currArr, (elemStr, idx)) => 
          val e = elemStr(1)
          if (e != ' ') {  
            val currStack = arr(idx)
            currArr.updated(idx, Seq(e) ++ currStack)
          } else currArr
      }
    }
  }
  def parseMoves(input: Seq[String]): Seq[Move] = {
    input.map {
      case regex(n, from, to) => Move(n.toInt, from.toInt, to.toInt)
    }
  }
  def parse(m: Seq[String], input: Seq[String]): (Array[Seq[Char]], Seq[Move]) = {
    if (input.head.isEmpty) {
      println("empty line")
      (parseCrates(m), parseMoves(input.tail))
    } else parse(m ++ Seq(input.head), input.tail)
  }
  
  def print(crates: Array[Seq[Char]]): Unit = {
    val arrArr = crates.map(_.toArray)
    val maxHeight = crates.maxBy(_.length).length
    (0 until maxHeight).reverse.foreach { idx =>
      arrArr.foreach { arr =>
        Try(arr(idx)) match {
          case Success(c) => printf(s"[$c] ")
          case Failure(_) => printf(s"    ")
        }
      }
      println()
    }
    crates.zipWithIndex.foreach {
      case (_, idx) => printf(s" $idx  ")
    }
    println()
  }
  
  def partOne: String = {
    val (crates, moves) = parse(Seq.empty, input)
    println(crates.toSeq)
    print(crates)
    println("_______________")
    println(moves)
    val res = moves.zipWithIndex.foldLeft(crates){
      case (currCrates, (nextMove, idx)) => 
        val fromStack = currCrates(nextMove.src - 1)
        val toStack = currCrates(nextMove.dest - 1)
        val (fromNow, moving) = fromStack.splitAt(fromStack.length - nextMove.n)
        val toNow = toStack ++ moving.reverse
        val updated = currCrates.updated(nextMove.src - 1, fromNow)
          .updated(nextMove.dest - 1, toNow)
        println(s"Move ${idx + 1}: ${nextMove.rendered}")
        print(updated)
        updated
    }
    res.map(_.last).mkString("")
  }
  
  def partTwo: String = {
    val (crates, moves) = parse(Seq.empty, input)
    println(crates.toSeq)
    print(crates)
    println("_______________")
    println(moves)
    val res = moves.zipWithIndex.foldLeft(crates){
      case (currCrates, (nextMove, idx)) =>
        val fromStack = currCrates(nextMove.src - 1)
        val toStack = currCrates(nextMove.dest - 1)
        val (fromNow, moving) = fromStack.splitAt(fromStack.length - nextMove.n)
        val toNow = toStack ++ moving
        val updated = currCrates.updated(nextMove.src - 1, fromNow)
          .updated(nextMove.dest - 1, toNow)
        println(s"Move ${idx + 1}: ${nextMove.rendered}")
        print(updated)
        updated
    }
    res.map(_.last).mkString("")
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}