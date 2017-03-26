import scala.io.Source

object Signal extends App {
  val filename = "input.txt"

  def sortByFrequency(list: Seq[Char]): Seq[Char] = {
    list.groupBy(identity)
      .map(group => (group._1, group._2.length))
      .toList
      .sortBy(group => (-group._2, group._1))
      .map(_._1)
  }

  def solvePartB(lines: Seq[String]): String = {
    lines.transpose.map {
      sortByFrequency(_).last
    }.mkString
  }

  def solvePartA(lines: Seq[String]): String = {
    lines.transpose.map {
      sortByFrequency(_).head
    }.mkString
  }

  try {
    val lines = Source.fromFile(filename).getLines.toList
    println("Part A:")
    println(solvePartA(lines))
    println("Part B:")
    println(solvePartB(lines))
  } catch {
    case exception: Exception => println(exception)
  }
}