package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.{ mutable, SortedMap }

/**
 * Map with a single entry optimized for speed.
 * @author Ricardo Leon
 * @param k single key
 * @param v single value
 * @param key2int key transformation.  Used to build new maps.
 * @tparam K key type
 * @tparam V value type
 */
private[immutable] final class NavigableMap1[K, +V](private[this] val k: K, private[this] val v: V)(implicit val ordering: Ordering[K],
  protected[this] val key2int: KeyTransformation[K],
  protected[this] val keyManifest: ClassManifest[K],
  protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMap[K, V] {

  // private[this] val seems to offer a small performance increase
  private[this] val valOption = Some(v)

  private[this] val keyOption = Some(k)

  override def size = 1

  def iterator = Iterator.single(k -> v)

  def -(key: K) =
    if (key == k)
      NavigableMap.empty[K, V]
    else
      this

  override def firstKey = k

  override def lastKey = k

  /**
   * @param from  The lower-bound (inclusive) of the ranged projection.
   *              <code>None</code> if there is no lower bound.
   * @param until The upper-bound (exclusive) of the ranged projection.
   *              <code>None</code> if there is no upper bound.
   */
  def rangeImpl(from: Option[K], until: Option[K]) =
    (from.getOrElse(k), until) match {
      case (lower, None) if ordering.gteq(lower, k) => this
      case (lower, Some(upper)) if ordering.lt(upper, k) && ordering.gteq(lower, k) => this
      case _ => NavigableMap.empty(ordering, key2int, keyManifest, valueManifest)
    }

  override def keysIterator = Iterator.single(k)

  override def valuesIterator = Iterator.single(v)

  override def toIterator: Iterator[(K, V)] = Iterator.single(k -> v)

  override def isEmpty = false

  final def get(key: K) =
    if (k != key) None else valOption

  def floorKey(key: K) =
    if (ordering.gteq(key, k)) keyOption else None

  def higherKey(key: K) =
    if (ordering.lt(key, k)) keyOption else None

  def ceilingKey(key: K) =
    if (ordering.lteq(key, k)) keyOption else None

  def lowerKey(key: K) =
    if (ordering.gt(key, k)) keyOption else None

  def +[V1 >: V: ClassManifest](kv: (K, V1)): NavigableMap[K, V1] = NavigableMap((k -> v), kv)

  def +[V1 >: V](kv: (K, V1)): SortedMap[K, V1] = SortedMap((k -> v), kv)

  override def toString() = "NavigableMap(%s -> %s)".format(k, v)
}