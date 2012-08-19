package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.immutable.Seq
import collection.SortedMap

/**
 * Created with IntelliJ IDEA.
 * User: rleon
 * Date: 8/18/12
 * Time: 6:54 PM
 * To change this template use File | Settings | File Templates.
 */
class SortedArrayMultiMap[K, V](protected[this] val sortedKeys: Array[K],
  protected[this] val sortedValues: Array[V])(implicit val ordering: Ordering[K],
    protected[this] val key2int: KeyTransformation[K],
    protected[this] val keyManifest: ClassManifest[K],
    protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMultiMap[K, V] with SortedArrayMap[K, V] {

  def -(key: K): NavigableMultiMap[K, V] = null

  def get(key: K): Option[Seq[V]] = null

  def iterator: Iterator[(K, Seq[V])] = null

  def contains[V1 >: V](k: K, v: V1): Boolean = false

  def flat: (Iterable[K], Iterable[V]) = null

  def flatEntriesIterable: Iterable[(K, V)] = null

  def flatIterable: Iterable[V] = null

  def totalSize: Int = 0

  def +[B1 >: Seq[V]](kv: (K, B1)): SortedMap[K, B1] = null

  def rangeImpl(from: Option[K], until: Option[K]): NavigableMultiMap[K, V] = null

  // ------- SortedMap and Map methods ------- //
  override def size = sizeInt - duplicates

  override def firstKey: K = lowestKey

  override def lastKey: K = highestKey

}
