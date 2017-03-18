object Hacking extends App {
  val input = "ojvtpuvg"
  val messageDigest = java.security.MessageDigest.getInstance("md5")


  def solvePartB(id: String) = {

  }

  def solvePartA(id: String): String = {
    Iterator.from(0, 1)
      .map {id + _}
      .map { string =>
        val bytes = messageDigest.digest(string.getBytes)
        new java.math.BigInteger(1, bytes).toString(16)
      }
      .filter {_.length == 27}
      .take(8)
      .foldLeft("") {_ + _(0)}
  }

  try {
    println("Part A:")
    println(solvePartA(input))
    println("Part B:")
    solvePartB(input)
  } catch {
    case exception: Exception => println(exception)
  }
}
