import scala.collection.mutable.ListBuffer

object Utils {
  /**
   * Converts a negative index that represents an index from the end of a collection its associated positive index.
   *
   * @param index  The negative index (-1 means last index in collection).
   * @param length The length of the collection.
   * @return The non-negative index associated with the negative index given based on the length of the collection given.
   */
  def unInvertIndex(index: Int, length: Int): Int = {
    if (index < 0)
      length + index
    else index
  }

  /**
   * Splits the arguments given in the whitespace eliminated string into a list of argument.
   * Wrap the argument in double quotes to include whitespace and use the escape character to include double quotes or a specific whitespace character.
   *
   * example: arg1 arg\ 2 "arg 3" argWithQuote\"InIt = arg1, arg 2, arg 3, argWithQuote"InIt
   *
   * @param input The string to split.
   * @return The string split into arguments.
   */
  def splitArgs(input: String): Array[String] = {
    /** Whether the iterator is inside a set of quotes. */
    var inQuotes = false
    /** Whether the iterator is on a character immediately following an escape character. */
    var escaped = false

    val output = new ListBuffer[StringBuilder]
    output.addOne(new StringBuilder)

    val iterator = input.iterator

    while (iterator.hasNext) {
      val char = iterator.next()

      if (escaped) {
        output.last.append(char)
        escaped = false
      } else {
        char match {
          case '\\' => escaped = true
          case '"' => inQuotes = !inQuotes
          case _ =>
            if (char.isWhitespace && !inQuotes) {
              if (output.last.nonEmpty)
                output.addOne(new StringBuilder)
            } else {
              output.last.append(char)
            }
        }
      }
    }

    output.map(s => s.toString()).filter(s => s.nonEmpty).toArray
  }
}
