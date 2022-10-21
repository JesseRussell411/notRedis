import javax.sql.DataSource
import scala.io.StdIn.readLine

/**
 * Not-Redis submission for Quiq code challenge
 *
 * @author Jesse Russell
 */
object Main extends App {
  val data = new DataStore

  while (true) {
    // get command from user
    val command = readLine("> ")

    // break command into arguments
    val args = Utils.splitArgs(command)

    // if the command isn't blank, then execute the command
    if (args.nonEmpty) {
      /** The output of the command */
      val result: String | Option[String] = try {
        args(0).toLowerCase() match {

          // get key's value store
          case "get" =>
            if (args.length != 2) throw new WrongNumberOfArgumentsException

            data.get(args(1))

          // set key's value store
          case "set" =>
            if (args.length != 3) throw new WrongNumberOfArgumentsException

            data.set(args(1), args(2))
            "OK"

          // delete key
          case "del" =>
            if (args.length < 2) throw new WrongNumberOfArgumentsException

            data.del(args.slice(1, args.length): _*).toString

          // push onto a stack store
          case "lpush" =>
            if (args.length < 3) throw new WrongNumberOfArgumentsException
            val slice = args.slice(2, args.length)
            data.lpush(args(1), slice: _*)
            (args.length - 2).toString

          // pop from a stack value
          case "lpop" =>
            args.length match {
              case 2 => data.lpop(args(1))
              case 3 =>
                val count = args(2).toInt

                if (count == 0)
                  "(empty array)"
                else
                  data.lpop(args(1), count) match {
                    case Some(output) => output.zipWithIndex
                      // make output look nice
                      .map((s, i) => s"${i.+(1)} \"$s\"")
                      .mkString("\n")
                    case None => None
                  }
            }

          // get a sub range from a stack store
          case "lrange" =>
            if (args.length != 4) throw new WrongNumberOfArgumentsException

            data.lrange(args(1), args(2).toInt, args(3).toInt) match {
              case Some(range) => range.zipWithIndex
                // make output look nice
                .map((s, i) => s"${i + 1}) \"$s\"")
                .mkString("\n")
              case None => None
            }

          // put a key/value into a map store
          case "hset" =>
            if (args.length < 4 || args.length % 2 != 0) throw new WrongNumberOfArgumentsException

            val key = args(1)
            var newEntryCount = 0
            for (i <- 2 until args.length by 2) {
              if (data.hset(key, args(i), args(i + 1)).isEmpty) newEntryCount += 1
            }

            newEntryCount.toString

          // get a value from a key in a map store
          case "hget" =>
            if (args.length != 3) throw new WrongNumberOfArgumentsException

            data.hget(args(1), args(2))

          // unknown command
          case _ => s"Unknown Command '${args(0)}' with args: ${args.slice(1, args.length).map(a => s"'$a'").mkString(" ")}"
        }
      } catch {
        case e: WrongTypeException => "ERROR: Operation against a key holding the wrong kind of value"
        case e: WrongNumberOfArgumentsException => s"ERROR: Wrong number of arguments for command ${args(0)}"
      }

      // print the result.
      // Non-empty option means data, like the value returned by get
      // string means info like the OK returned by set
      // None means nil like the result of get on a key that doesn't exists
      println(result match {
        case s: String => s
        case Some(s) => '"' + s + '"'
        case None => "(nil)"
      })
    }
  }
}
