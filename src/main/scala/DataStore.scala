import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer, Stack}

/**
 * Redis like data store.
 */
class DataStore {
  /** Can be either a value (string), stack, or map */
  type Store = String | mutable.Stack[String] | mutable.HashMap[String, String]
  /** Maps keys to values. The value can be a String (basic value), Stack (for Lpush, Lpop, and Lrange), or Hashmap (for hget and hset) */
  private val data = new mutable.HashMap[String, Store]
  /** read-write lock for some basic thread safety. Would probably be better to lock on a per-key basis, especially if this is for caching. */
  private val lock = new ReentrantReadWriteLock()

  /**
   * Stores the value at the key.
   * Overwrites current value.
   *
   * @param key   The key at which to store the value.
   * @param value The value to map the key to.
   * @return
   */
  def set(key: String, value: String): Option[Store] = {
    lock.writeLock().lock()
    try {
      data.put(key, value)
    } finally {
      lock.writeLock().unlock()
    }
  }

  /**
   * Gets the value stored at the key.
   *
   * @param key The key from which to get the value.
   * @return An Option containing the value if it was found or None if the key does not exist.
   * @throws WrongTypeException If The store is not a basic value (string) but is instead a stack, map, or other non-basic store.
   */
  def get(key: String): Option[String] = {
    lock.readLock().lock()
    try {
      data.get(key) match {
        case Some(store) => store match {
          case basic: String => Some(basic)
          case _ => throw new WrongTypeException
        }
        case None => None
      }
    } finally {
      lock.readLock().unlock()
    }
  }

  /**
   * Deletes the given keys and their values.
   *
   * @param keys the keys to delete
   * @return The number of keys that where found and deleted. Keys that don't exists don't count.
   */
  def del(keys: String*): Int = {
    lock.writeLock().lock()
    try {
      var deleteCount = 0
      for (key <- keys) {
        if (data.remove(key).nonEmpty) deleteCount += 1
      }
      deleteCount
    } finally {
      lock.writeLock().unlock()
    }
  }

  /**
   * Pushes the elements onto the stack at the key. If there is no stack at the key, creates a new stack and pushes the elements onto that.
   *
   * @param key      The key at which the stack is stored.
   * @param elements The elements to push onto the stack.
   * @throws WrongTypeException If the store at the key is not a stack.
   */
  def lpush(key: String, elements: String*): Unit = {
    lock.writeLock().lock()
    try {
      data.getOrElseUpdate(key, new mutable.Stack[String]) match {
        case stack: mutable.Stack[String] => stack.pushAll(elements)
        case _ => throw new WrongTypeException
      }
    } finally {
      lock.writeLock().unlock()
    }
  }

  /**
   * Pops the head elements off the stack and returns it.
   *
   * @param key The key at which the stack is stored
   * @return Option containing the value popped off the stack or None if the stack was empty or if no stack was found at the key.
   * @throws WrongTypeException If the store at the key isn't a stack.
   */
  def lpop(key: String): Option[String] = {
    lock.writeLock().lock()
    try {
      data.get(key) match {
        case Some(store) =>
          store match {
            case stack: mutable.Stack[String] => if (stack.nonEmpty) Some(stack.pop()) else None
            case _ => throw new WrongTypeException
          }
        case None => None
      }
    } finally {
      lock.writeLock().unlock()
    }
  }

  /**
   * Pops multiple values off the stack at the given key.
   *
   * @param key   The key at which the stack is stored.
   * @param count How many values to pop off of the stack.
   * @return Option containing a list of the values that where popped off the stack or None if no stack was found at the key.
   * @throws WrongTypeException If the store at the key wasn't a stack.
   */
  def lpop(key: String, count: Int): Option[List[String]] = {
    lock.writeLock().lock()
    try {
      data.get(key) match {
        case Some(store) =>
          store match {
            case stack: mutable.Stack[String] =>

              /** List to collect the values popped from the stack. */
              val result = new ListBuffer[String]

              /** How many values have been popped from the stack. */
              var popCount = 0
              while (popCount < count && stack.nonEmpty) {
                result.addOne(stack.pop())
                popCount += 1
              }

              if (result.isEmpty) None else Some(result.toList)
            case _ => throw new WrongTypeException
          }
        case None => None
      }
    } finally {
      lock.writeLock().unlock()
    }
  }

  /**
   * Gets a range of values from the stack at the given key.
   *
   * @param key   The key at which the stack is stored.
   * @param start The first value to get. Negative numbers indicates indexes from the end (-1 means last index).
   * @param stop  The last value to get. Negative numbers indicates indexes from the end (-1 means last index).
   * @return Option containing the range of values or none if no stack was found at the key.
   * @throws WrongTypeException If the store at the key wasn't a stack.
   */
  def lrange(key: String, start: Int, stop: Int): Option[Iterable[String]] = {
    lock.readLock().lock()
    try {
      data.get(key) match {
        case Some(store) => store match {
          case stack: mutable.Stack[String] => Some(stack.slice(
            Utils.unInvertIndex(start, stack.size),
            Utils.unInvertIndex(stop, stack.size) + 1).toList)
          case _ => throw new WrongTypeException
        }
        case None => None
      }
    } finally {
      lock.readLock().unlock()
    }
  }

  /**
   * Stores a key/value pair in the map at the key. If no map is stored at the key, a new one will be made and the key/value pair will be stored in the new map.
   *
   * @param key   The key at which the map is stored.
   * @param field The field at which to store the value in the map.
   * @param value The value to store in the map.
   * @return Option containing the previous value stored at the field in the map or None if either the field wasn't already defined in the map or if no map was found at the given key.
   */
  def hset(key: String, field: String, value: String): Option[String] = {
    lock.readLock().lock()
    try {
      data.getOrElseUpdate(key, new mutable.HashMap[String, String]()) match {
        case map: mutable.HashMap[String, String] => map.put(field, value)
        case _ => throw new WrongTypeException
      }
    } finally {
      lock.readLock().unlock()
    }
  }

  /**
   * Gets the value associated with the field in the map stored at the key.
   *
   * @param key   The key at which the map is stored.
   * @param field The field at which to get the value from the map.
   * @return Option containing the value associated with the field in the map or None if the field isn't found in the map or if no map is stored at the key.
   * @throws WrongTypeException If the store at the key isn't a map.
   */
  def hget(key: String, field: String): Option[String] = {
    lock.readLock().lock()
    try {
      data.get(key) match {
        case Some(store) => store match {
          case map: mutable.HashMap[String, String] => map.get(field)
          case _ => throw new WrongTypeException
        }
        case None => None
      }
    } finally {
      lock.readLock().unlock()
    }
  }
}
