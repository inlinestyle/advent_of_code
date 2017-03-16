import scala.io.Source

object Obscurity extends App {
  val filename = "input.txt"
  // parsing "aaaaa-bbb-z-y-x-123[abxyz]", where the first part has an
  // arbitrary number of hyphenated sections
  val parser = "([\\w-]+)-(\\d+)\\[(\\w{5})\\]".r

  def solvePartB(triangleLines: Seq[String]): Integer = {
    0
  }

  def solvePartA(lines: Seq[String]): Integer = {
    lines.foldLeft(0) {
      case (acc, parser(name, id, checksum)) =>
        println(name
          .split('-')
          .flatten
          .groupBy(_.toChar)
          .toList
          .map(group => (group._1, group._2.length))
          .sortWith((a, b) => if (a._2 == b._2) a._1 < b._1 else a._2 > b._2))
        println(s"$name : $id : $checksum")
        acc
    }
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
