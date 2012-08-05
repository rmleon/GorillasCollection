package gorillas.collection.immutable

import collection._
import collection.generic.CanBuildFrom
import gorillas.collection.generic.KeyTransformation
import gorillas.collection.mutable.NavigableMapBuilder
import scala.Iterator
import scala.Traversable
import scala.Predef._
import gorillas.collection.immutable.NavigableMap.KeyFilteredNavigableMap

/**
 * @author Ricardo Leon
 *         Immutable map that uses a binary search with the aid of a precomputed index to speed up access.
 *         It aims to obtain read access time O(1) and has a worst performance of O(log n).
 *         Storage is O(n)
 * @tparam K key type
 * @tparam V value type
 */
trait NavigableMap[K, +V] extends SortedMap[K, V] with SortedMapLike[K, V, NavigableMap[K, V]] with Immutable {
  self =>

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
  def +[V1 >: V](kv: (K, V1))(implicit value1Manifest: ClassManifest[V1]): NavigableMap[K, V1]

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
   * @return true if that is an instance of SortedMap (or NavigableMap)
   */
  override def canEqual(that: Any): Boolean = that.isInstanceOf[SortedMap[K, V]]

  def updated[V1 >: V: ClassManifest](key: K, value: V1): NavigableMap[K, V1] = this + ((key, value))

  /**
   * Without recreating the underlying collection, filters the results of get, iterator, ceilingKey, higherKey, lowerKey, forEach, contains, and size.
   * Size will be recalculated each time it is called.
   * @param p the predicate.  This implementation doesn't assume that p is immutable, but p should not change in the middle of a method.
   * @return returns a collection that filters the underlying collection affecting the following methods: get, iterator, ceilingKey, higherKey, lowerKey, forEach, contains, and size.
   */
  def withFilterKeys(p: K => Boolean): NavigableMap[K, V] = new NavigableMap.KeyFilteredNavigableMap[K, V](this, p)
}

object NavigableMap extends NavigableMapFactory[NavigableMap] {

  implicit def canBuildFrom[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest] = new NavigableMapCanBuildFromSameKeyValue[K, V]

  class KeyFilteredNavigableMap[K, V](underlying: NavigableMap[K, V], p: K => Boolean)(implicit val ordering: Ordering[K],
    protected[this] val key2int: KeyTransformation[K],
    protected[this] val keyManifest: ClassManifest[K],
    protected[this] val valueManifest: ClassManifest[V]) extends NavigableMap[K, V] {

    def -(key: K): NavigableMap[K, V] = underlying.-(key)

    def +[V1 >: V](kv: (K, V1)): SortedMap[K, V1] = {
      val builder = SortedMap.newBuilder[K, V1]
      builder.sizeHint(underlying.size + 1)
      builder ++= (iterator) += (kv)
      builder result ()
    }

    def +[V1 >: V](kv: (K, V1))(implicit value1Manifest: ClassManifest[V1]): NavigableMap[K, V1] = underlying.updated(kv._1, kv._2)(value1Manifest)

    def get(key: K): Option[V] = if (p(key)) underlying.get(key) else None

    def iterator: Iterator[(K, V)] = underlying.iterator.withFilter(kv => p(kv._1))

    def ceilingKey(key: K): Option[K] = {
      val originalKeyOpt = underlying.ceilingKey(key)
      if (originalKeyOpt.isEmpty || p(originalKeyOpt.get))
        originalKeyOpt
      else
        underlying.higherKey(key)
    }

    def floorKey(key: K): Option[K] = {
      val originalKeyOpt = underlying.floorKey(key)
      if (originalKeyOpt.isEmpty || p(originalKeyOpt.get))
        originalKeyOpt
      else
        underlying.lowerKey(key)
    }

    def higherKey(key: K): Option[K] = {
      var found = underlying.higherKey(key)
      while (found.isDefined && !p(found.get))
        found = underlying.higherKey(found.get)
      found
    }

    def lowerKey(key: K): Option[K] = {
      var found = underlying.lowerKey(key)
      while (found.isDefined && !p(found.get))
        found = underlying.lowerKey(found.get)
      found
    }

    override def foreach[U](f: ((K, V)) => U) { iterator.foreach(f) }

    override def contains(key: K): Boolean = p(key) && underlying.contains(key)

    def rangeImpl(from: Option[K], until: Option[K]): NavigableMap[K, V] = underlying.rangeImpl(from, until).withFilterKeys(p)

    override def size: Int = underlying.keys.count(p)
  }

}

