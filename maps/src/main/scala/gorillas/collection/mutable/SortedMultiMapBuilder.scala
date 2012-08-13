package gorillas.collection.mutable

import gorillas.collection.generic.KeyTransformation
import gorillas.collection.immutable.SortedMultiMap

/**
 * @author Ricardo Leon
 * @param ordering keys will be sorted according to this ordering
 * @param key2int the keys will be "grouped" for quick retrieval according to this transformation.  The ordering must be consistent with the transformation.
 * @param keyManifest needed to create arrays
 * @param valueManifest need to create arrays
 * @tparam K key type
 * @tparam V value type
 */
class SortedMultiMapBuilder[K, V](implicit ordering: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V])
  extends scala.collection.mutable.Builder[(K, V), SortedMultiMap[K, V]] {

  // @TODO
  def +=(elem: (K, V)): this.type = null

  // @TODO
  def clear() {}

  // @TODO
  def result(): SortedMultiMap[K, V] = null
}
