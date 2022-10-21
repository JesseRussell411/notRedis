import java.lang.invoke.WrongMethodTypeException
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer, Stack}

class DataStore {
  type Store = String | mutable.Stack[String] | mutable.HashMap[String, String]
  /** Maps keys to values. The value can be a String (basic value), Stack (for Lpush, Lpop, and Lrange), or Hashmap (for hget and hset) */
  val data = new mutable.HashMap[String, Store]

  /**
   * Stores the value at the key.
   * Overwrites current value.
   *
   * @param key
   * @param value
   * @return
   */
  def set(key: String, value: String): Option[Store] = {
    data.put(key, value)
  }

  /**
   * Gets the value stored at the key.
   *
   * @throws WrongTypeException
   * @param key
   * @return
   */
  def get(key: String): Option[String] = {
    data.get(key) match {
      case Some(store) => store match {
        case basic: String => Some(basic)
        case _ => throw new WrongTypeException
      }
      case None => None
    }
  }

  def del(keys: String*): Int = {
    var deleteCount = 0
    for (key <- keys) {
      if (data.remove(key).nonEmpty) deleteCount += 1
    }
    deleteCount
  }

  def lpush(key: String, elements: String*): Unit = {
    data.getOrElseUpdate(key, new Stack[String]) match {
      case stack: Stack[String] => stack.pushAll(elements)
      case _ => throw new WrongTypeException
    }
  }

  def lpop(key: String): Option[String] = {
    data.get(key) match {
      case Some(store) =>
        store match {
          case stack: Stack[String] => if (stack.nonEmpty) Some(stack.pop()) else None
          case _ => throw new WrongTypeException
        }
      case None => None
    }
  }

  def lpop(key: String, count: Int): Option[List[String]] = {
    data.get(key) match {
      case Some(store) =>
        store match {
          case stack: mutable.Stack[String] =>
            val result = new ListBuffer[String]
            var i = 0
            while (i < count && stack.nonEmpty) {
              result.addOne(stack.pop())
              i += 1
            }

            if (result.isEmpty) None else Some(result.toList)
          case _ => throw new WrongTypeException
        }
      case None => None
    }
  }

  def lrange(key: String, start: Int, stop: Int): Option[Iterable[String]] = {
    data.get(key) match {
      case Some(store) => store match {
        case stack: Stack[String] => Some(stack.slice(start, stop + 1).toList)
        case _ => throw new WrongTypeException
      }
      case None => None
    }
  }

  def hset(key: String, field: String, value: String): Option[String] = {
    data.getOrElseUpdate(key, new mutable.HashMap[String, String]()) match {
      case map: mutable.HashMap[String, String] => map.put(field, value)
      case _ => throw new WrongMethodTypeException
    }
  }

  def hget(key: String, field: String): Option[String] = {
    data.get(key) match {
      case Some(store) => store match {
        case map: HashMap[String, String] => map.get(field)
        case _ => throw new WrongMethodTypeException
      }
      case None => None
    }
  }
}
