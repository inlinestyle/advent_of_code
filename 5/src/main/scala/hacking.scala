object Hacking extends App {
  val input = "ojvtpuvg"

  val messageDigest = java.security.MessageDigest.getInstance("md5")

  val emptyPassword: Seq[Option[Char]] = Seq(None, None, None, None, None, None, None, None)

  def fiveZeroesIterator(id: String): Iterator[String] = {
    Iterator.from(0)
      .map {id + _}
      .map { string =>
        val bytes = messageDigest.digest(string.getBytes)
        new java.math.BigInteger(1, bytes).toString(16)
      }
      .filter {_.length <= 27}
      .map {"%27s".format(_).replace(' ', '0')}
  }

  def solvePartB(id: String): String = {
    fiveZeroesIterator(id)
      .scanLeft(emptyPassword) { (password, hash) =>
        val index = hash(0).asDigit
        if (index < 8 && password(index) == None)
          password.updated(index, Some(hash(1)))
        else password
      }
      .dropWhile {password => !password.forall(_ != None)}
      .take(1)
      .flatten
      .flatten
      .mkString
  }

  def solvePartA(id: String): String = {
    fiveZeroesIterator(id).take(8).foldLeft("") {_ + _(0)}
  }

  try {
    println("Part A:")
    println(solvePartA(input))
    println("Part B:")
    println(solvePartB(input))
  } catch {
    case exception: Exception => println(exception)
  }
}
