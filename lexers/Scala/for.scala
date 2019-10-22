

/** Turn command line arguments to uppercase */
object Ma$in {
  def main(args: Array[String]) {
    val res = for (a <- args) yield a.toUpperCase
    println("Arguments: " + res.toString)
  }
}
