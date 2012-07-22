package gorillas.collection.immutable

import collection.{IterableLike, SortedMap, SortedMapLike}
import gorillas.collection.generic.KeyTransformation
import gorillas.collection.mutable.NavigableMapBuilder

/**
 * @author Ricardo Leon
 *         Immutable map that uses a binary search with the aid of a precomputed index to speed up access.
 *         It aims to obtain read access time O(1) and has a worst performance of O(log n).
 *         Storage is O(n)
 * @tparam K key type
 * @tparam V value type
 */
trait NavigableMap[K, +V] extends SortedMapLike[K, V, NavigableMap[K, V]] with SortedMap[K, V] with Immutable with IterableLike[(K, V), NavigableMap[K, V]] {

  protected[this] implicit def key2int: KeyTransformation[K]

  protected[this] implicit def keyManifest: ClassManifest[K]

  protected[this] implicit def valueManifest: ClassManifest[V]

  override protected[this] def newBuilder: NavigableMapBuilder[K, V] =
    NavigableMap.newBuilder[K, V]

  override def empty: NavigableMap[K, V] = NavigableMap.empty[K, V]

  /**
   * Unlike SortedMap, NavigableMap requires the value type's ClassManifest to create the backing arrays.
   * @param kv the new entry
   * @tparam V1 the new value type
   * @return a new map with the new entry
   */
  def +[V1 >: V : ClassManifest](kv: (K, V1)): NavigableMap[K, V1]

  /**
   * @param key lookup key
   * @return The greatest key less than or equal to the given key
   */
  def floorKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The least key strictly greater than the given key
   */
  def higherKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The least key greater than or equal to the given key
   */
  def ceilingKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The greatest key strictly less than the given key
   */
  def lowerKey(key: K): Option[K]

  /**
   * @param that
   * @return true if that is an instance of SortedMap (or NavigableMap)
   */
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Traversable[(K, V)]]
}

object NavigableMap extends NavigableMapFactory

