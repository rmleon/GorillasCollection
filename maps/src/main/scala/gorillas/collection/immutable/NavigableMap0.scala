package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import scala.collection.immutable.SortedSet
import scala.collection.SortedMap

/**
 * Just an empty map.
 * @param ordering key ordering used to build new maps.
 * @param key2int key transformation used to build new maps.
 * @tparam K key type
 * @tparam V value type
 */
private[immutable] final class NavigableMap0[K, +V](implicit val ordering: Ordering[K],
                                                    protected[this] val key2int: KeyTransformation[K],
                                                    protected[this] val keyManifest: ClassManifest[K],
                                                    protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMap[K, V] {

  override def size = 0

  def get(key: K) = None

  def floorKey(key: K) = None

  def higherKey(key: K) = None

  def ceilingKey(key: K) = None

  def lowerKey(key: K) = None

  override def keySet = SortedSet.empty[K]

  override def isEmpty = true

  override def keysIterator: Iterator[K] = Iterator.empty

  override def contains(key: K) = false

  override def empty = this

  override def valuesIterator: Iterator[V] = Iterator.empty

  override def toIterator: Iterator[(K, V)] = Iterator.empty

  override def count(p: ((K, V)) => Boolean) = 0

  def iterator: Iterator[(K, V)] = Iterator.empty

  def -(key: K) = this

  def +[V1 >: V : ClassManifest](kv: (K, V1)): NavigableMap[K, V1] = NavigableMap(kv)

  def +[V1 >: V](kv: (K, V1)): SortedMap[K, V1] = SortedMap(kv)

  def rangeImpl(from: Option[K], until: Option[K]) = this

  override def toString = "NavigableMap()"
}