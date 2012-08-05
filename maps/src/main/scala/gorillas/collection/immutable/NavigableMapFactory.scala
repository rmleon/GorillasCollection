package gorillas.collection.immutable

import gorillas.collection.mutable.NavigableMapBuilder
import gorillas.collection.generic.KeyTransformation
import collection.generic.CanBuildFrom

/**
 * @author Ricardo Leon
 */
abstract class NavigableMapFactory[CC[K, V] <: NavigableMap[K, V]] {
  type Coll = CC[_, _]

  def empty[K, V](implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMap[K, V] =
    new NavigableMap0[K, V]

  def apply[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest](elements: (K, V)*): NavigableMap[K, V] =
    (newBuilder[K, V] ++= elements).result()

  def newBuilder[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest]: NavigableMapBuilder[K, V] =
    new NavigableMapBuilder[K, V]

  private[collection] def single[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest](singleEntry: (K, V)): NavigableMap[K, V] =
    new NavigableMap1[K, V](singleEntry._1, singleEntry._2)

  class NavigableMapCanBuildFromSameKeyValue[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest] extends CanBuildFrom[Coll, (K, V), NavigableMap[K, V]] {
    def apply() = NavigableMap.newBuilder[K, V]

    def apply(from: Coll) = NavigableMap.newBuilder[K, V]
  }
}
