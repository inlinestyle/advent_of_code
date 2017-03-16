import scala.io.Source

object Triangles extends App {
  val filename = "input.txt"
  val parser = "\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)".r

  def isValid(a: String, b: String, c: String): Boolean = {
    val sorted = Vector(a, b, c).map(_.toInt).sorted
    sorted(2) < (sorted(1) + sorted(0))
  }

  def solvePartB(triangleLines: Seq[String]): Integer = {
    val splitted = triangleLines.map(_.trim.split("\\s+"))
    val mapped = splitted.map(_(0)) ++ splitted.map(_(1)) ++ splitted.map(_(2))
    mapped.grouped(3).foldLeft(0) {
      case (acc, Seq(a, b, c)) =>
        if (isValid(a, b, c)) acc + 1 else acc
    }
  }

  def solvePartA(triangleLines: Seq[String]): Integer = {
    triangleLines.foldLeft(0) {
      case (acc, parser(a, b, c)) =>
        if (isValid(a, b, c)) acc + 1 else acc
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
