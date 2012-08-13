package gorillas.collection.immutable

import collection.{ IterableLike, SortedMap, SortedMapLike }
import gorillas.collection.mutable.SortedMultiMapBuilder
import collection.generic.{ Subtractable, Sorted }

/**
 * A map with sorted keys that can hold multiple values under the same key.
 * @author Ricardo Leon
 */
trait SortedMultiMap[K, +V] extends Sorted[K, SortedMultiMap[K, V]]
  with IterableLike[(K, V), SortedMultiMap[K, V]] with PartialFunction[K, Iterable[V]] with Iterable[(K, V)]
  with Subtractable[K, SortedMultiMap[K, V]] with Immutable {

  def empty: SortedMultiMap[K, V] = null

  override protected[this] def newBuilder: SortedMultiMapBuilder[K, V] = null

  //extends SortedMap[K, Iterable[V]] with Immutable {
  /**
   * @return all sorted values.  The ordering will be given by the key.  Multiple values under the same key keep their insertion order.
   */
  def values: Iterable[V]

  /**
   * @return all sorted values.  The ordering will be given by the key.  Multiple values under the same key keep their insertion order.
   */
  def valuesIterator: Iterator[V]

  /**
   * @return All sorted keys including all the keys are that are repeated.
   */
  def keys: Iterable[K]

  /**
   * @return All sorted keys including all the keys are that are repeated.
   */
  def keysIterator: Iterator[K]

  /**
   * @return All entries sorted by its key.
   */
  def entries: Iterable[(K, V)]

  /**
   * @return All entries sorted by its key.
   */
  def entriesIterator: Iterator[(K, V)]

  //def - (kv: (K, V)): SortedMultiMap[K, V]

  /**
   * @return Number of unique keys in this map.
   */
  def uniqueKeysSize: Int

}
