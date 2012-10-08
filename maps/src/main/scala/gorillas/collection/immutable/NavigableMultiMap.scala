package gorillas.collection.immutable

import collection.{ IterableLike, SortedMap, SortedMapLike }
import gorillas.collection.mutable.NavigableMultiMapBuilder
import collection.generic.{ Subtractable, Sorted }
import collection.immutable.Seq
import gorillas.collection.generic.KeyTransformation
import annotation.migration

/**
 * A map with sorted keys that can hold multiple values under the same key.
 * @author Ricardo Leon
 */
trait NavigableMultiMap[K, +V] extends NavigableMapLike[K, Seq[V]] with SortedMap[K, Seq[V]] with SortedMapLike[K, Seq[V], NavigableMultiMap[K, V]] with Immutable {

  protected[this] implicit def key2int: KeyTransformation[K]

  protected[this] implicit def keyManifest: ClassManifest[K]

  protected[this] implicit def valueManifest: ClassManifest[V]

  override protected[this] def newBuilder: NavigableMultiMapBuilder[K, V] =
    NavigableMultiMap.newBuilder[K, V]

  override def empty: NavigableMultiMap[K, V] = NavigableMultiMap.empty[K, V]

  /**
   * @return all entries sorted by the key.  The insertion order is kept within for values with the same key.
   */
  def flatEntries: Iterator[(K, V)]

  /**
   * @return unzipped keys and values sorted by the key.  The insertion order is kept within for values with the same key.
   */
  def flat: (Iterator[K], Iterator[V])

  def contains[V1 >: V](k: K, v: V1): Boolean

  /**
   * @return Number of all values inside the collections
   */
  def totalSize: Int

  override def stringPrefix: String = "NavigableMultiMap"

}

object NavigableMultiMap extends NavigableMultiMapFactory[NavigableMultiMap] {

  implicit def canBuildFrom[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest] = new NavigableMultiMapCanBuildFromSameKeyValue[K, V]

}