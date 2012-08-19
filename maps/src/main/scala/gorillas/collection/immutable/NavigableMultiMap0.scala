package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import collection.immutable.{ SortedSet, Seq }
import collection.SortedMap

private[immutable] final class NavigableMultiMap0[K, +V](implicit val ordering: Ordering[K],
  protected[this] val key2int: KeyTransformation[K],
  protected[this] val keyManifest: ClassManifest[K],
  protected[this] val valueManifest: ClassManifest[V])
  extends NavigableMultiMap[K, V] {

  override def size = 0

  def get(key: K) = None

  def flat: (Iterable[K], Iterable[V]) = (Iterable.empty -> Iterable.empty)

  def flatEntriesIterable = Iterable.empty

  def flatIterable = Iterable.empty

  def totalSize: Int = 0

  def floorKey(key: K) = None

  def higherKey(key: K) = None

  def ceilingKey(key: K) = None

  def lowerKey(key: K) = None

  override def keySet = SortedSet.empty[K]

  override def isEmpty = true

  override def keysIterator: Iterator[K] = Iterator.empty

  override def contains(key: K) = false

  override def empty = this

  override def valuesIterator = Iterator.empty

  override def toIterator = Iterator.empty

  def contains[V1 >: V](k: K, v: V1): Boolean = false

  def iterator = Iterator.empty

  def -(key: K) = this

  def +[V1 >: Seq[V]](kv: (K, V1)): SortedMap[K, V1] = SortedMap(kv)

  def rangeImpl(from: Option[K], until: Option[K]) = this

  override def toString() = "NavigableMultiMap()"
}
