package gorillas.collection.mutable

import gorillas.collection.immutable.{ SortedArrayNavigableMap, NavigableMap }
import gorillas.util.PairSorting
import gorillas.collection.generic.KeyTransformation
import collection.{ GenTraversableOnce, mutable }

/**
 * @author Ricardo Leon
 * @param ordering self explanatory
 * @param key2int self explanatory
 * @tparam K map entry's key type
 * @tparam V map entry's value type
 */
final class NavigableMapBuilder[K, V](implicit ordering: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V])
  extends AbstractNavigableMapBuilder[K, V] with mutable.Builder[(K, V), NavigableMap[K, V]] {

  override def ++=(xs: TraversableOnce[(K, V)]): this.type =
    ++=(xs.asInstanceOf[GenTraversableOnce[(K, V)]])

  override def clear() {
    keys.clear()
    values.clear()
  }

  override def sizeHint(size: Int) {
    keys.sizeHint(size)
    values.sizeHint(size)
  }

  override def result(): NavigableMap[K, V] = {
    keys.size match {
      case 0 => NavigableMap.empty[K, V]
      case 1 => NavigableMap.single((keys(0), values(0)))
      case _ =>
        val keysArray = keys.toArray
        val valuesArray = values.toArray
        PairSorting.mergeSort(keysArray, valuesArray)
        new SortedArrayNavigableMap[K, V](keysArray, valuesArray)
    }
  }
}
