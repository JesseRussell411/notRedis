import javax.sql.DataSource
import scala.io.StdIn.readLine

object Main extends App {
  val data = new DataStore

  while (true) {
    val command = readLine()
    val args = command.split("[^\\S\\r\\n]").filter(s => s.nonEmpty)

    if (args.nonEmpty) {
      val result: String =
        try {
          args(0).toLowerCase() match {
            case "get" =>
              if (args.length != 2) throw new WrongNumberOfArgumentsException

              data.get(args(1)) match {
                case Some(value) => value
                case None => "(nil)"
              }
            case "set" =>
              if (args.length != 3) throw new WrongNumberOfArgumentsException
              
              data.set(args(1), args(2))
              "OK"
            case _ => s"Unknown Command '${args(0)}' with args: ${args.slice(1, args.length).map(a => s"'$a'").mkString(" ")}"
          }
        } catch {
          // string ripped straight from redis output
          case e: WrongTypeException => "(error) WRONGTYPE Operation against a key holding the wrong kind of value"
          case e: WrongNumberOfArgumentsException => s"Wrong number of arguments for command ${args(0)}"
        }
      println(result)
    }
  }
}
