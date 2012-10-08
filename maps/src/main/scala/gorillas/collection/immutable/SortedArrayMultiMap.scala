package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.immutable.Seq
import collection.SortedMap

/**
 *
 * @param sortedKeys input array with the sorted keys.  The builder discards this array; it should never be modified.
 * @param sortedValues values sorted in the same order as the keys.  Again, the builder discards this array so that it can never be modified.
 * @param ordering key ordering
 * @param key2int a transformation to integer to enable quick lookups
 * @param keyManifest to create key arrays
 * @param valueManifest to create value arrays
 * @tparam K key type
 * @tparam V associated value type
 * @author Ricardo Leon
 */
class SortedArrayMultiMap[K, V](protected[this] val sortedKeys: Array[K],
  protected[this] val sortedValues: Array[V])(implicit val ordering: Ordering[K],
    protected[this] val key2int: KeyTransformation[K],
    protected[this] val keyManifest: ClassManifest[K],
    protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMultiMap[K, V] with SortedArrayMap[K, V] {

  // TODO
  def -(key: K): NavigableMultiMap[K, V] = null

  // TODO
  final def get(key: K): Option[Seq[V]] = {
  val hintIdx = hintIndex(key2int.transform(key))
    if (hintIdx < 0 || hintIdx >= sizeInt) // Out of range
      None
    else
      binarySearch(key, hints(hintIdx), hints(hintIdx + 1)) // It turns out that inline parameters generate less bytecode
  }

  // TODO
  def iterator: Iterator[(K, Seq[V])] = null

  // TODO
  def contains[V1 >: V](k: K, v: V1): Boolean = false

  // TODO  (create a class for the arrays)
  def flat = null

  // TODO
  def totalSize: Int = sizeInt

  // TODO
  def +[B1 >: Seq[V]](kv: (K, B1)): SortedMap[K, B1] = null

  // TODO
  def rangeImpl(from: Option[K], until: Option[K]): NavigableMultiMap[K, V] = null

  // ------- SortedMap and Map methods ------- //
  override def size = sizeInt - duplicates

  override def firstKey: K = lowestKey

  override def lastKey: K = highestKey

}
