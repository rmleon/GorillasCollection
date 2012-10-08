package gorillas.collection.immutable

import annotation.tailrec
import gorillas.collection.generic.KeyTransformation

/**
 * Very fast access NavigableMap with low footprint.
 * Assumes that at least two elements are present.
 * All methods inherited from SortedMap have the same behavior as SortedMap.  However, it also holds the values under repeated keys.
 * @author Ricardo Leon
 * @tparam K key type
 * @tparam V associated value type
 */
private[immutable] trait SortedArrayMap[K, V] {
  /**
   * sortedKeys keys ordered according to "ordering"
   */
  protected[this] val sortedKeys: Array[K]

  /**
   * sortedValues corresponding values sorted in the same order as the keys
   */
  protected[this] val sortedValues: Array[V]

  implicit val ordering: Ordering[K]

  /**
   * a transformation from key to int used to do fast lookups.  As much as possible, it should be equally spaced to make the lookup operations very fast.
   */
  implicit protected[this] val key2int: KeyTransformation[K]

  implicit protected[this] val keyManifest: ClassManifest[K]

  implicit protected[this] val valueManifest: ClassManifest[V]

  assert(sortedKeys.length == sortedValues.length, "Keys and values should have the same size")

  /**
   * Defining a private val that duplicates size allows me to avoids "invoke virtual".
   */
  @inline protected[this] final val sizeInt: Int = sortedKeys.length
  assert(sizeInt > 1, "Empty or single maps should use a different implementation")

  /**
   * hintIdx operates using Long values.  Using this value instead of "size" saves an extra "i2l" and "invoke virtual" calls.
   */
  private[this] final val sizeLong: Long = sizeInt

  protected[this] final def lowestKey = sortedKeys(0)

  protected[this] final def highestKey = sortedKeys(sizeInt - 1)

  private[this] final val lowestKeyLong: Long = key2int.transform(lowestKey)

  private[this] final val highestKeyLong: Long = key2int.transform(highestKey)

  assert(lowestKeyLong < highestKeyLong, "The entries should be sorted in ascending order (max:%d, min:%d, max:%s, min:%s)" format (highestKeyLong, lowestKeyLong, highestKey, lowestKey))
  // Note: to be able to handle duplicated keys, this would need to be assert(min <= max)

  private[this] val range: Long = 1l + highestKeyLong - lowestKeyLong
  assert(range > 0, "Range can't be 0 or a negative value")

  /**
   * Do not modify this array to keep this class immutable.
   */
  protected[this] final val hints: Array[Int] = {
    val result = Array.ofDim[Int](sizeInt + 1) // result(0) should be -1
    result(0) = -1
    var i = 0
    while (i < sizeInt) {
      val key = key2int.transform(sortedKeys(i))
      result(hintIndex(key) + 1) = i
      i += 1
    }
    // Fill in the gaps with the last position
    var lastFilledPos = 0
    i = 1
    while (i <= sizeInt) {
      if (result(i) == 0)
        result(i) = lastFilledPos
      else
        lastFilledPos = result(i)
      i += 1
    }
    result
  }

  protected[this] val duplicates = {
    var result = 0
    var i = 1
    while (i < sortedKeys.length) {
      if (sortedKeys(i - 1) == sortedKeys(i)) result += 1
      i += 1
    }
    result
  }

  /**
   * @param key transformed key value
   * @return the index in the hints array where the "key" position might be located
   */
  protected[this] final def hintIndex(key: Int): Int = (((key - lowestKeyLong) * sizeLong) / range).toInt // The final keyword is redundant here (unlike "size" and "binarySearch").

  // The code below saves 40ms for the Int case
  //  private[this] val comparator: util.Comparator[K] = new util.Comparator[K] {
  //    def direction(p1: K, p2: K): Int = ordering.direction(p1, p2)
  //  }
  //
  //  final def get0(key: K): Option[V] = {
  //    val hintIdx = hintIndex(key2int.transform(key))
  //    if (hintIdx < 0 || hintIdx > size0) // Out of range
  //      None
  //    else {
  //      val index: Int = if (key.isInstanceOf[Int])
  //        Arrays.binarySearch(sortedKeys.asInstanceOf[Array[Int]], hints(hintIdx) + 1, hints(hintIdx + 1) + 1, key.asInstanceOf[Int])
  //      else
  //        -1
  //      if (index != -1)
  //        Some(sortedValues(index))
  //      else
  //        None
  //    }
  //  }

  /**
   * @param key lookup key
   * @return The least key strictly greater than the given key
   */
  final def higherKey(key: K) = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx >= sizeInt || ordering.gteq(key, highestKey))
      None
    else if (hintIdx < 0 || ordering.lt(key, lowestKey))
      Some(lowestKey)
    else {
      var index = binarySearchClosest(key, hints(hintIdx), hints(hintIdx + 1))
      while (ordering.gteq(key, sortedKeys(index))) // this can be optimized
        index += 1
      Some(sortedKeys(index))
    }
  }

  /**
   * @param key lookup key
   * @return The greatest key strictly less than the given key
   */
  final def lowerKey(key: K) = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx >= sizeInt)
      Some(highestKey)
    else if (hintIdx < 0 || ordering.lteq(key, lowestKey)) //  1 3 5: 0-> None, 1 -> None, 2 -> 1, 3 -> 1, 4 -> 3, 5 -> 3, 6 -> 5, 1000->5
      None
    else {
      var index = binarySearchClosest(key, hints(hintIdx), hints(hintIdx + 1))
      while (ordering.lteq(key, sortedKeys(index))) // this can be optimized
        index -= 1
      Some(sortedKeys(index))
    }
  }

  /**
   * @param key lookup key
   * @return The least key greater than or equal to the given key
   */
  final def ceilingKey(key: K) = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx >= sizeInt)
      None
    else if (hintIdx < 0 || ordering.lteq(key, lowestKey))
      Some(lowestKey)
    else {
      var index = binarySearchClosest(key, hints(hintIdx), hints(hintIdx + 1))
      while (ordering.gt(key, sortedKeys(index))) // this can be optimized
        index += 1
      Some(sortedKeys(index))
    }
  }

  /**
   * @param key lookup key
   * @return The greatest key less than or equal to the given key
   */
  final def floorKey(key: K) = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx >= sizeInt)
      Some(highestKey)
    else if (hintIdx < 0 || ordering.lt(key, lowestKey))
      None
    else {
      var index = binarySearchClosest(key, hints(hintIdx), hints(hintIdx + 1))
      while (ordering.lt(key, sortedKeys(index))) // this can be optimized
        index -= 1
      Some(sortedKeys(index))
    }
  }

  protected[this] def getClosestIndex(key: K): Int = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx < 0)
      0
    else if (hintIdx >= sizeInt) // Out of range
      sizeInt
    else
      binarySearchClosest(key, hints(hintIdx), hints(hintIdx + 1))
  }

  /**
   * @param key key to search
   * @return -1 if not found, or the key index otherwise
   */
  protected[this] def indexOf(key: K): Int = {
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx < 0 || hintIdx >= sizeInt)
      -1
    else
      binarySearchIndex(key, hints(hintIdx), hints(hintIdx + 1))
  }

  /**
   * Lookup.
   * @param key lookup key
   * @param minIdx initial index in the range (exclusive).  To include the first element with index 0, pass -1.  Assertion: -1 <= minIdx <= maxIdx
   * @param maxIdx last index in the range (inclusive). Assertion: 0 <= maxIdx < length
   * @return the associated value if found
   */
  @tailrec protected[this] final def binarySearch(key: K, minIdx: Int, maxIdx: Int): Option[V] = { // Notes: @inline didn't see to have any effect.  Final might seem redundant but it is not (I checked with javap).
    //    assert(minIdx <= maxIdx, "Error: minIdx <= maxIdx always")
    //    assert(maxIdx < length, "Error: maxIdx < length always")
    //    assert(minIdx >= -1, "Error: minIdx should be -1 or higher (-1 indicates that the first element could be index=0)")
    if (minIdx < maxIdx) {
      val middleIdx = (2 + maxIdx + minIdx) >> 1
      val comparison = ordering.compare(key, sortedKeys(middleIdx))
      if (comparison == 0)
        Some(sortedValues(middleIdx))
      else if (comparison < 0)
        binarySearch(key, minIdx, middleIdx - 1)
      else // if (comparison > 0)
        binarySearch(key, middleIdx, maxIdx)
    } else
      None
  }

  /**
   * Lookup but it returns the closest position (even if the key is not there)
   * @param key lookup key
   * @param minIdx initial index in the range (exclusive).  To include the first element with index 0, pass -1.  Assertion: -1 <= minIdx <= maxIdx
   * @param maxIdx last index in the range (inclusive). Assertion: 0 <= maxIdx < length
   * @return the closest entry it could find.  minIdx <= returnValue <= maxIdx
   */
  @tailrec private[this] final def binarySearchClosest(key: K, minIdx: Int, maxIdx: Int): Int = {
    //    assert(minIdx <= maxIdx, "Error: minIdx <= maxIdx always")
    //    assert(maxIdx < length, "Error: maxIdx < length always")
    //    assert(minIdx >= -1, "Error: minIdx should be -1 or higher (-1 indicates that the first element could be index=0)")
    if (minIdx < maxIdx) {
      val middleIdx = (2 + maxIdx + minIdx) >> 1
      val comparison = ordering.compare(key, sortedKeys(middleIdx))
      if (comparison == 0)
        middleIdx
      else if (comparison < 0)
        binarySearchClosest(key, minIdx, middleIdx - 1)
      else // if (comparison > 0)
        binarySearchClosest(key, middleIdx, maxIdx)
    } else
      maxIdx
  }

  /**
   * Lookup but it returns the closest position (even if the key is not there)
   * @param key lookup key
   * @param minIdx initial index in the range (exclusive).  To include the first element with index 0, pass -1.  Assertion: -1 <= minIdx <= maxIdx
   * @param maxIdx last index in the range (inclusive). Assertion: 0 <= maxIdx < length
   * @return -1 if it can't find the key
   */
  @tailrec private[this] final def binarySearchIndex(key: K, minIdx: Int, maxIdx: Int): Int = {
    if (minIdx < maxIdx) {
      val middleIdx = (2 + maxIdx + minIdx) >> 1
      val comparison = ordering.compare(key, sortedKeys(middleIdx))
      if (comparison == 0)
        middleIdx
      else if (comparison < 0)
        binarySearchIndex(key, minIdx, middleIdx - 1)
      else // if (comparison > 0)
        binarySearchIndex(key, middleIdx, maxIdx)
    } else
      -1
  }

  final def flatEntries = new Iterator[(K, V)] {

    private[this] var position = -1

    @inline def hasNext: Boolean = position + 1 < sizeInt

    def next(): (K, V) = {
      if (!hasNext) {
        throw new NoSuchElementException("next on empty iterator")
      }
      position += 1
      while (position + 1 < sizeInt && sortedKeys(position) == sortedKeys(position + 1)) // This forces the iterator to get the last key
        position += 1
      (sortedKeys(position) -> sortedValues(position))
    }
  }
}
