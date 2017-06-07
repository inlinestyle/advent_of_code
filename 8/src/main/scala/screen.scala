import scala.io.Source

object Screen extends App {
  val filename = "input.txt"

  type Screen = List[List[String]]
  val numRows = 3
  val numColumns = 50
  val screen: Screen = List.fill(numRows)(List.fill(numColumns)("."))

  def printScreen(screen: Screen) = {
    screen.foreach(row => println(row.mkString("")))
  }

  val rectPattern = "rect (\\d+)x(\\d+)".r
  val rotateColumnPattern = "rotate column x=(\\d+) by (\\d+)".r
  val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r

  def rect(screen: Screen, columns: Int, rows: Int): Screen = {
    screen.zipWithIndex.map {
      case (row, j) =>
        if (j < rows) row.zipWithIndex.map {
          case (column, i) =>
            if (i < columns) "#"
            else column
        }
        else row
    }
  }

  def rotateColumn(screen: Screen, x: Int, by: Int): Screen = {
    screen.zipWithIndex.map {
      case (row, j) =>
        row.zipWithIndex.map {
          case (column, i) =>
            if (i == x) screen(Math.floorMod(j - by, numRows))(i)
            else column
        }
    }
  }

  def rotateRow(screen: Screen, y: Int, by: Int): Screen = {
    screen.zipWithIndex.map {
      case (row, j) =>
        if (j == y) row.zipWithIndex.map {
          case (_, i) =>
            screen(j)(Math.floorMod(i - by, numColumns))
        }
        else row
    }
  }

  def solvePartA(lines: Seq[String]): String = {
    val resultScreen = lines.foldLeft(screen) {
      case (acc, rectPattern(columns, rows)) =>
        rect(acc, columns.toInt, rows.toInt)
      case (acc, rotateColumnPattern(x, by)) =>
        rotateColumn(acc, x.toInt, by.toInt)
      case (acc, rotateRowPattern(y, by)) =>
        rotateRow(acc, y.toInt, by.toInt)
    }
    printScreen(resultScreen)
    resultScreen.flatten.foldLeft(0) {
      case (acc, pixel) =>
        if (pixel == "#") acc + 1 else acc
    }.toString
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