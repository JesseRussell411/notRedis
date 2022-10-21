import javax.sql.DataSource
import scala.io.StdIn.readLine

object Main extends App {
  val data = new DataStore

  while (true) {
    val command = readLine()
    val args = command.split("[^\\S\\r\\n]").filter(s => s.nonEmpty)

    if (args.nonEmpty) {
      val result: Option[String] | String =
        try {
          args(0).toLowerCase() match {
            case "get" =>
              if (args.length != 2) throw new WrongNumberOfArgumentsException

              data.get(args(1))
            case "set" =>
              if (args.length != 3) throw new WrongNumberOfArgumentsException

              data.set(args(1), args(2))
              "OK"
            case "lpush" =>
              if (args.length < 3) throw new WrongNumberOfArgumentsException

              data.lpush(args(1), args.slice(2, args.length))
              (args.length - 2).toString
            case "lpop" =>
              args.length match {
                case 2 => data.lpop(args(1))
                case 3 => data.lpop(args(1), args(2).toInt) match {
                  case Some(output) => output.zipWithIndex.map((s, i) => s"${i.+(1)} \"$s\"").mkString("\n")
                  case None => None
                }
              }
            case "lrange" =>
              if (args.length != 4) throw new WrongNumberOfArgumentsException

              data.lrange(args(1), args(2).toInt, args(3).toInt) match {
                case Some(range) => range.zipWithIndex.map((s, i) => s"${i + 1}) \"$s\"").mkString("\n")
                case None => None
              }

            case _ => s"Unknown Command '${args(0)}' with args: ${args.slice(1, args.length).map(a => s"'$a'").mkString(" ")}"
          }
        } catch {
          case e: WrongTypeException => "(error) WRONGTYPE Operation against a key holding the wrong kind of value"
          case e: WrongNumberOfArgumentsException => s"Wrong number of arguments for command ${args(0)}"
        }
      println(result match {
        case s: String => s
        case Some(s) => s
        case None => "(nil)"
      })
    }
  }
}
