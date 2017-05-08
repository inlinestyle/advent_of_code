import scala.io.Source

object Protocol extends App {
  val filename = "input.txt"

  val partAGoodPattern =        ".*(\\w)(?!\\1)(\\w)\\2\\1.*".r
  val partABadPattern = "\\[[^\\]]*(\\w)(?!\\1)(\\w)\\2\\1[^\\]]*\\]".r

  val partBPattern1 =                              ".*(\\w)(?!\\1)(\\w)\\1.*\\[[^\\]]*\\2\\1\\2[^\\]]*\\].*".r
  val partBPattern2 = "\\[[^\\]]*\\2\\1\\2[^\\]]*\\].*(\\w)(?!\\1)(\\w)\\1.*".r
  def solvePartB(lines: Seq[String]): String = {
    lines.count { line =>
      partBPattern1.findAllIn(line).nonEmpty || partBPattern2.findAllIn(line).nonEmpty
    }.toString
  }

  def solvePartA(lines: Seq[String]): String = {
    lines.count { line =>
      partAGoodPattern.findAllIn(line).nonEmpty && partABadPattern.findAllIn(line).isEmpty
    }.toString
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