package puzzle8

import scala.io.Source

object Solution {
  val input: Seq[String] = Source.fromResource("input8.txt")
    .getLines
    .toSeq
  val array = input
    .mkString("")
    .toCharArray
    .map(_.toString.toInt)

  val xLength = input.head.length
  val yLength = input.length
  
  def to1D(x: Int, y: Int): Int = {
    (y * xLength) + x
  }
  
  def to2D(i: Int): (Int, Int) = {
    val x = i % xLength
    val y = i / xLength
    (x, y)
  }
  
  def checkVertical(x: Int, y: Int, height: Int): Boolean = {
    val toCheckUp = ((0 until y).toSet).map {
      yc => to1D(x, yc)
    }
    val toCheckDown = ((y + 1 until yLength).toSet).map {
      yc => to1D(x, yc)
    }
    toCheckUp.forall(array(_) < height) || toCheckDown.forall(array(_) < height)
  }

  def checkHorizontal(x: Int, y: Int, height: Int): Boolean = {
    val toCheckLeft = ((0 until x).toSet).map {
      xc => to1D(xc, y)
    }
    val toCheckRight = ((x + 1 until xLength).toSet).map {
      xc => to1D(xc, y)
    }
    toCheckLeft.forall(array(_) < height) || toCheckRight.forall(array(_) < height)
  }
  
  def visible(coord: Int): Boolean = {
    val height = array(coord)
    val (x, y) = to2D(coord)
    x == 0 || x == xLength - 1 || y == 0 || y == yLength - 1 || checkHorizontal(x, y, height) || checkVertical(x, y, height)
  }
  
  def partOne: Int = {
    val outsideTrees = yLength * 2 + xLength * 2 - 4
    array.indices.count(visible)
  }

  def calculateVertical(x: Int, y: Int, height: Int): Int = {
    val toCheckUp = if (y != 0) (0 until y).map {
      yc => to1D(x, yc)
    } else Seq.empty
    
    val toCheckDown = if (y != yLength - 1) (y + 1 until yLength).map {
      yc => to1D(x, yc)
    } else Seq.empty
    
    toCheckUp.sorted.reverse.zipWithIndex.collectFirst {
      case (idx, n) if array(idx) >= height => n + 1
    }.getOrElse(toCheckUp.length) * toCheckDown.sorted.zipWithIndex.collectFirst {
        case (idx, n) if array(idx) >= height => n + 1
    }.getOrElse(toCheckDown.length)
  }
  
  def calculateHorizontal(x: Int, y: Int, height: Int): Int = {
    val toCheckLeft = if (x != 0) (0 until x).map {
      xc => to1D(xc, y)
    } else Seq.empty

    val toCheckRight = if (x != xLength - 1) (x + 1 until xLength).map {
      xc => to1D(xc, y)
    } else Seq.empty

    toCheckLeft.sorted.reverse.zipWithIndex.collectFirst {
      case (idx, n) if array(idx) >= height => n + 1
    }.getOrElse(toCheckLeft.length) * toCheckRight.sorted.zipWithIndex.collectFirst {
      case (idx, n) if array(idx) >= height => n + 1
    }.getOrElse(toCheckRight.length)
  }
  
  def calculate(coord: Int): Int = {
    val height = array(coord)
    val (x, y) = to2D(coord)
    val res = calculateHorizontal(x, y, height) * calculateVertical(x, y, height)
    res
  }

  def partTwo: Int = {
    calculate(array.indices.maxBy(calculate))
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}