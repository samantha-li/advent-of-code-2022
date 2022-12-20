package puzzle7

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

// This one was a struggle. Don't look at it, it's pretty gross.

object Solution {
  val input: Seq[String] = Source.fromResource("input7.txt")
    .getLines
    .toSeq
  
  val dirRegex = "dir ([a-z]+)".r
  val fileRegex = "([0-9]+) ([a-z\\.]+)".r
  val cdRegex = "\\$ cd ([/a-z]+)".r
  val lsRegex = "\\$ ls".r
  
  @tailrec
  def parseDirectory(allNodes: Map[String, (String, Int, Set[String])], currentDir: String, input: Seq[String], level: Int): Map[String, (String, Int, Set[String])] = {
    if (input.isEmpty) allNodes
    else {
      input.head match {
        case lsRegex() => parseDirectory(allNodes, currentDir, input.tail, level)
        case dirRegex(name) =>
          val fullPath = currentDir + "/" + name
          val (parent, size, subFolders) = allNodes.getOrElse(currentDir, ("", 0, Set.empty[String]))
          val newSubFolders = subFolders ++ Set(fullPath)
          val updatedMap = allNodes ++ Map(
            currentDir -> (parent, size, newSubFolders)
          )
          val newNodes = if (!allNodes.contains(fullPath)) updatedMap ++ Map(fullPath -> (currentDir, 0, Set.empty[String]))
          else updatedMap
          parseDirectory(
            newNodes,
            currentDir,
            input.tail,
            level
          )
        case fileRegex(size, _) =>
          val (parent, mapSize, subDirs) = allNodes.getOrElse(currentDir, ("", 0, Set.empty[String]))
          val newSize = mapSize + size.toInt
          val elem: (String, Int, Set[String]) = (parent, newSize, subDirs)
          parseDirectory(allNodes ++ Map(currentDir -> elem), currentDir, input.tail, level)
        case cdRegex(name) =>
          val fullPath = currentDir + "/" + name
          parseDirectory(
            allNodes,
            fullPath,
            input.tail,
            level + 1
          )
        case "$ cd .." =>
          val target = allNodes.getOrElse(currentDir, throw new Exception(s"WTF"))._1
          parseDirectory(allNodes, target, input.tail, level - 1)
      }
    }
  }
  
  @tailrec
  def process(allNodes: Seq[(String, (String, Int, Set[String]))], collectAll: Map[String, Int]): Map[String, Int] = {
    if (allNodes.isEmpty) collectAll 
    else {
      val (remaining, mapped) =
        allNodes.foldLeft((Seq.empty[(String, (String, Int, Set[String]))], collectAll)) {
        case ((acc, updatedMap), (dirName, (_, fileSize, subFolders))) if subFolders.diff(collectAll.keySet).isEmpty => 
          (acc, updatedMap ++ Map(dirName -> (fileSize + subFolders.foldLeft(0){
            case (sum, folder) => collectAll.getOrElse(folder, throw new Exception("WTF 2")) + sum
          })))
        case ((acc, updatedMap), elem) => (acc ++ Seq(elem), updatedMap)
      }
      process(remaining, mapped)
    }
  }
  
  def partOne: Int = {
    val mapped = parseDirectory(Map("/" -> ("", 0, Set.empty)), "", input, 0)
    val processed = process(mapped.toSeq, Map.empty)
    processed.collect {
      case (_, size) if size <= 100000 => size
    }.sum
  }
  
  def partTwo: Int = {
    val mapped = parseDirectory(Map("/" -> ("", 0, Set.empty)), "", input, 0)
    val processed = process(mapped.toSeq, Map.empty)
    val totalSize = processed.getOrElse("//", throw new Exception("BIG BOOM"))
    val unused = 70000000 - totalSize
    val needed = 30000000
    val toDelete = needed - unused
    processed.collect {
      case (_, size) if size >= toDelete => size
    }.min
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}