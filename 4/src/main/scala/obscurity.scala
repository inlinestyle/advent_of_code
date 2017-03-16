import scala.io.Source

object Obscurity extends App {
  type ParsedLine = (String, Integer, String)

  val filename = "input.txt"
  // parsing "aaaaa-bbb-z-y-x-123[abxyz]", where the first part has an
  // arbitrary number of hyphenated sections
  val parser = "([\\w-]+)-(\\d+)\\[(\\w{5})\\]".r

  def filterLines(lines: Seq[String]): Seq[ParsedLine] = {
    lines.foldLeft(Seq[ParsedLine]()) {
      case (acc, parser(name, id, checksum)) =>
        val test = name
          .split('-')
          .flatten
          .groupBy(_.toChar) // not sure why .groupBy(_) doesn't compile
          .toList
          .map(group => (group._1, group._2.length))
          .sortWith((a, b) => if (a._2 == b._2) a._1 < b._1 else a._2 > b._2)
          .map(_._1)
          .take(5)
          .mkString
        if (test == checksum)
          acc :+ ((name, id.toInt, checksum): ParsedLine)
        else acc
    }
  }

  def solvePartB(lines: Seq[String]): Integer = {
    val offset = 'a'.toInt
    filterLines(lines).foldLeft(0) {
      case (acc, (name, id, _)) =>
        println(name.map(char =>
          if (char == '-')
            ' '
          else (((char.toInt - offset + id) % 26) + offset).toChar).mkString)
        acc
    }
  }

  def solvePartA(lines: Seq[String]): Integer = {
    filterLines(lines).foldLeft(0) {
      case (acc, (_, id, _)) =>
        acc + id
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
