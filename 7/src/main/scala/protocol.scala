import scala.io.Source

object Protocol extends App {
  val filename = "input.txt"

  val goodPattern = ".*(\\w)(?!\\1)(\\w)\\2\\1.*".r
  val badPattern = "\\[[^\\]]*(\\w)(\\w)\\2\\1[^\\]]*\\]".r

  def solvePartB(lines: Seq[String]): String = {
    ""
  }

  def solvePartA(lines: Seq[String]): String = {
    lines.filter { line =>
      goodPattern.findAllIn(line).length > 0 && badPattern.findAllIn(line).length == 0
    }.length.toString
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