import scala.io.Source
import scala.math.abs

object Security extends App {
  val filename = "input.txt"

  val partAButtonsMap = Map(
    (-1, 1)  -> "1",
    (0, 1)   -> "2",
    (1, 1)   -> "3",
    (-1, 0)  -> "4",
    (0, 0)   -> "5",
    (1, 0)   -> "6",
    (-1, -1) -> "7",
    (0, -1)  -> "8",
    (1, -1)  -> "9"
  )

  val partBButtonsMap = Map(
    (0, 2)   -> "1",
    (-1, 1)  -> "2",
    (0, 1)   -> "3",
    (1, 1)   -> "4",
    (-2, 0)  -> "5",
    (-1, 0)  -> "6",
    (0, 0)   -> "7",
    (1, 0)   -> "8",
    (2, 0)   -> "9",
    (-1, -1) -> "A",
    (0, -1)  -> "B",
    (1, -1)  -> "C",
    (0, -2)  -> "D"
  )

  val directionsMap = Map(
    'D' -> (0, -1),
    'L' -> (-1, 0),
    'R' -> (1, 0),
    'U' -> (0, 1)
  )

  def normalize(int: Int): Int = {
    if (abs(int) > 1) (int / abs(int)) else int
  }

  val add: ((Int, Int), (Int, Int)) => (Int, Int) = {
    case ((a, b), (c, d)) => (a + c, b + d)
  }

  def partAAdd(pointA: (Int, Int), pointB: (Int, Int)): (Int, Int) = {
    val (x, y) = add(pointA, pointB)
    (normalize(x), normalize(y))
  }

  def partBAdd(pointA: (Int, Int), pointB: (Int, Int)): (Int, Int) = {
    val (x, y) = add(pointA, pointB)
    if ((abs(x) + abs(y)) > 2) pointA else (x, y)
  }

  def solve(buttonLines: Seq[String],
            buttonsMap: Map[(Int, Int), String],
            add: ((Int, Int), (Int, Int)) => (Int, Int)
           ): String = {
    val inverseButtonsMap = buttonsMap.map(_.swap)

    buttonLines.foldLeft(("", inverseButtonsMap("5"))) {
      case ((buttons, startPosition), buttonLine) =>
        val buttonPosition = buttonLine.foldLeft(startPosition) {
          (position, direction) =>
            add(position, directionsMap(direction))
        }
        (buttons + buttonsMap(buttonPosition), buttonPosition)
    }._1
  }

  try {
    val lines = Source.fromFile(filename).getLines.toList
    println("Part A:")
    println(solve(lines, partAButtonsMap, partAAdd))
    println("Part B:")
    println(solve(lines, partBButtonsMap, partBAdd))
  } catch {
    case exception: Exception => println(exception)
  }

}
