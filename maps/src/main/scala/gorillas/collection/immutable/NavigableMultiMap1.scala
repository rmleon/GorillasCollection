package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.SortedMap
import collection.immutable.Seq
import gorillas.collection.generic.KeyTransformation

/**
 * Single entry map.
 * @author Ricardo Leon
 * @param k single key
 * @param v single value
 * @param key2int key transformation.  Used to build new maps.
 * @tparam K key type
 * @tparam V value type
 */
private[immutable] final class NavigableMultiMap1[K, +V](private[this] val k: K, private[this] val v: V)(implicit val ordering: Ordering[K],
  protected[this] val key2int: KeyTransformation[K],
  protected[this] val keyManifest: ClassManifest[K],
  protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMultiMap[K, V] {

  // private[this] val seems to offer a small performance increase
  private[this] val valOption = Some(Seq(v))

  private[this] val keyOption = Some(k)

  def flat: (Seq[K], Seq[V]) = (Seq(k) -> Seq(v))

  def flatEntriesIterable = Iterable(k -> v)

  def flatIterable = Iterable(v)

  def totalSize = 1

  override def size = 1

  def iterator = Iterator.single(k -> Seq(v))

  def -(key: K) =
    if (key == k)
      NavigableMultiMap.empty[K, V]
    else
      this

  override def firstKey = k

  override def lastKey = k

  def contains[V1 >: V](k1: K, v1: V1): Boolean = k1 == k && v1 == v

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
      case _ => NavigableMultiMap.empty(ordering, key2int, keyManifest, valueManifest)
    }

  override def keysIterator = Iterator.single(k)

  override def valuesIterator = Iterator.single(Seq(v))

  override def toIterator = Iterator.single(k -> Seq(v))

  def contains[V1 >: V](kv: (K, V1)) = k == kv._1 && v == kv._2

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

  def +[V1 >: V: ClassManifest](kv: (K, V1)): NavigableMultiMap[K, V1] = NavigableMultiMap[K, V1]((k -> v), kv)

  def +[B1 >: Seq[V]](kv: (K, B1)): SortedMap[K, B1] = SortedMap(k -> Seq(v), kv)
}