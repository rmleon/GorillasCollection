package gorillas.collection.mutable

import gorillas.collection.generic.KeyTransformation
import gorillas.collection.immutable.SortedMultiMap

class SortedMultiMapBuilder[K, V](implicit ordering: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V])
  extends scala.collection.mutable.Builder[(K, V), SortedMultiMap[K, V]] {

  def +=(elem: (K, V)): this.type = null

  def clear() {}

  def result(): SortedMultiMap[K, V] = null
}
