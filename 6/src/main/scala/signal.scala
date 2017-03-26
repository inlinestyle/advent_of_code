import scala.io.Source

object Signal extends App {
  val filename = "input.txt"

  def solvePartA(lines: Seq[String]): String = {
    lines.transpose.map {
      _.groupBy(identity)
        .map(group => (group._1, group._2.length))
        .toList
        .sortBy(group => (-group._2, group._1))
        .map(_._1)
        .head
    }.mkString
  }

  try {
    val lines = Source.fromFile(filename).getLines.toList
    println("Part A:")
    println(solvePartA(lines))
  } catch {
    case exception: Exception => println(exception)
  }
}