package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.{SortedMap, IndexedSeqLike, GenTraversableOnce}
import gorillas.collection.mutable.NavigableMapBuilder

final class SortedArrayNavigableMap[K, V](protected[this] val sortedKeys: Array[K],
                                          protected[this] val sortedValues: Array[V])
                                         (implicit val ordering: Ordering[K],
                                          protected[this] val key2int: KeyTransformation[K],
                                          protected[this] val keyManifest: ClassManifest[K],
                                          protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMap[K, V] with SortedArrayMap[K, V] {

  final def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {

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

  // -- Traversable/Iterable

  override def foreach[U](f: ((K, V)) => U) {
    var i = 0
    while (i < sizeInt) {
      while (i + 1 < sizeInt && sortedKeys(i) == sortedKeys(i + 1)) // Skip the duplicate keys
        i += 1
      f(sortedKeys(i) -> sortedValues(i))
      i += 1
    }
  }

  override def ++[V1 >: V: ClassManifest](xs: GenTraversableOnce[(K, V1)]): NavigableMap[K, V1] = {
    val builder = NavigableMap.newBuilder[K, V1]
    if (xs.isInstanceOf[IndexedSeqLike[_, _]])
      builder.sizeHint(xs.size + sortedKeys.length)
    builder ++= (sortedKeys, sortedValues, 0, sortedKeys.length)
    builder ++= xs
    builder result ()
  }

  final def -(key: K) = {
    if (!contains(key))
      this
    else {
      val builder = newBuilder
      builder.sizeHint(sizeInt - 1)
      var i = 0
      while (i < sizeInt) {
        val currentKey = sortedKeys(i)
        if (currentKey != key) {
          builder += ((currentKey, sortedValues(i)))
        }
        i += 1
      }
      builder result ()
    }
  }

  /**
   * @param from bottom key limit (inclusive.)  None indicates no bound.
   * @param until top key (exclusive.)  None indicates no limit.
   * @return a new navigable map with the given range.
   */
  def rangeImpl(from: Option[K], until: Option[K]) = {
    val lowerIndex = from match {
      case None => 0
      case Some(lower) =>
        getClosestIndex(lower)
    }
    val higherIndex = until match {
      case None => sizeInt
      case Some(higher) =>
        if (ordering.lt(highestKey, higher))
          sizeInt
        else {
          var index = getClosestIndex(higher)
          while (index >= 0 && ordering.gteq(sortedKeys(index), higher))
            index -= 1
          index + 1
        }
    }
    if (lowerIndex == 0 && higherIndex == sizeInt)
      this
    else if (higherIndex < lowerIndex)
      empty
    else {
      val builder = newBuilder
      builder ++= (sortedKeys.slice(lowerIndex, higherIndex), sortedValues.slice(lowerIndex, higherIndex))
      builder result ()
    }
  }

  // ------- Navigable Methods ------- //
  /**
   * Create NavigableMaps if V1's ClassManifest is available
   * @param kv new key value pair
   * @tparam V1 new value type
   * @return a new map with the element added
   */
  final def +[V1 >: V: ClassManifest](kv: (K, V1)): NavigableMap[K, V1] = {

    var insertionIndex = getClosestIndex(kv._1)
    while (insertionIndex < sizeInt && ordering.equiv(kv._1, sortedKeys(insertionIndex)))
      insertionIndex += 1

    val arrayInstanceOfV1 = sortedValues.asInstanceOf[Array[V1]]
    val builder = newBuilder[V1]
    builder.sizeHint(sizeInt + 1)
    builder ++= (sortedKeys, arrayInstanceOfV1, 0, insertionIndex) += kv ++= (sortedKeys, arrayInstanceOfV1, insertionIndex, sizeInt - insertionIndex)
    builder result ()
  }

  def +[V1 >: V](kv: (K, V1)): SortedMap[K, V1] = {
    val builder = SortedMap.newBuilder[K, V1]
    builder.sizeHint(sizeInt + 1)
    builder ++= iterator += kv
    builder result ()
  }

  final def get(key: K): Option[V] = { // "Final" might seem redundant but it is not (I checked with javap).  "Final" allows extra optimization at the cost of inheritance..
    val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx < 0 || hintIdx >= sizeInt) // Out of range
      None
    else
      binarySearch(key, hints(hintIdx), hints(hintIdx + 1)) // It turns out that inline parameters generate less bytecode
  }

  final override def contains(key: K) = get(key).isDefined

  // ------- SortedMap and Map methods ------- //
  override def size = sizeInt - duplicates

  override def firstKey: K = lowestKey

  override def lastKey: K = highestKey


}
