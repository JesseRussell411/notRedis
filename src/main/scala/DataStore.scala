import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

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
      case Some(value) => value match {
        case basic: String => Some(basic)
        case _ => throw new WrongTypeException
      }
      case None => None
    }
  }
}
