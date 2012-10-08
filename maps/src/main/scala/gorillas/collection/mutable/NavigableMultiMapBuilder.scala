package gorillas.collection.mutable

import gorillas.collection.generic.KeyTransformation
import gorillas.collection.immutable.{ SortedArrayMultiMap, NavigableMultiMap }
import collection.IndexedSeqLike
import collection.immutable.Seq
import gorillas.util.PairSorting

/**
 * @author Ricardo Leon
 * @param ordering keys will be sorted according to this ordering
 * @param key2int the keys will be "grouped" for quick retrieval according to this transformation.  The ordering must be consistent with the transformation.
 * @param keyManifest needed to create arrays
 * @param valueManifest need to create arrays
 * @tparam K key type
 * @tparam V value type
 */
final class NavigableMultiMapBuilder[K, V](implicit ordering: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V])
  extends AbstractNavigableMapBuilder[K, V] with scala.collection.mutable.Builder[(K, Seq[V]), NavigableMultiMap[K, V]] {

  override def clear() {
    keys.clear()
    values.clear()
  }

  override def sizeHint(size: Int) {
    keys.sizeHint(size)
    values.sizeHint(size)
  }

  def +=(elem: (K, Seq[V])): NavigableMultiMapBuilder.this.type = {
    val newValues = elem._2
    if (newValues.isInstanceOf[IndexedSeqLike[_, _]])
      sizeHint(newValues.size + keys.size + 1)

    newValues.foreach[Unit] {
      v: V =>
        keys += elem._1
        values += (v)
    }
    this
  }

  def result(): NavigableMultiMap[K, V] = {
    keys.size match {
      case 0 => NavigableMultiMap.empty[K, V]
      case 1 => NavigableMultiMap.single((keys(0), values(0)))
      case _ =>
        val keysArray = keys.toArray
        val valuesArray = values.toArray
        PairSorting.mergeSort(keysArray, valuesArray)
        new SortedArrayMultiMap[K, V](keysArray, valuesArray)
    }
  }

}
