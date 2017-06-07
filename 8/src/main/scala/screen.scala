import scala.io.Source

object Screen extends App {
  val filename = "input.txt"

  type Screen = List[List[String]]
  val screen: Screen = List.fill(3)(List.fill(50)("."))

  val rectPattern = "rect (\\d+)x(\\d+)".r
  val rotateColumnPattern = "rotate column x=(\\d+) by (\\d+)".r
  val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r

  def rect(screen: Screen, columns: String, rows: String): Screen = {
    screen
  }

  def rotateColumn(screen: Screen, x: String, by: String): Screen = {
    screen
  }

  def rotateRow(screen: Screen, y: String, by: String): Screen = {
    screen
  }

  def solvePartA(lines: Seq[String]): String = {
    val resultScreen = lines.foldLeft(screen) {
      case (acc, rectPattern(columns, rows)) =>
        rect(acc, columns, rows)
      case (acc, rotateColumnPattern(x, by)) =>
        rotateColumn(screen, x, by)
      case (acc, rotateRowPattern(y, by)) =>
        rotateRow(screen, y, by)
    }
    println(resultScreen)
    ""
  }

  try {
    val lines = Source.fromFile(filename).getLines.toList
    println("Part A:")
    println(solvePartA(lines))
    // println("Part B:")
    // println(solvePartB(lines))
  } catch {
    case exception: Exception => println(exception)
  }
}