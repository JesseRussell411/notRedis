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
}
