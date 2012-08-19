package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import gorillas.collection.mutable.{ NavigableMultiMapBuilder }
import collection.generic.CanBuildFrom
import collection.immutable.Seq

abstract class NavigableMultiMapFactory[CC[K, V] <: NavigableMultiMap[K, V]] {
  type Coll = CC[_, _]

  def empty[K, V](implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMultiMap[K, V] =
    new NavigableMultiMap0[K, V]

  def apply[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest](elements: (K, V)*): NavigableMultiMap[K, V] =
    (newBuilder[K, V] ++= elements).result()

  def newBuilder[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest]: NavigableMultiMapBuilder[K, V] =
    new NavigableMultiMapBuilder[K, V]

  private[collection] def single[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest](singleEntry: (K, V)): NavigableMultiMap[K, V] =
    new NavigableMultiMap1[K, V](singleEntry._1, singleEntry._2)

  class NavigableMultiMapCanBuildFromSameKeyValue[K: Ordering: ClassManifest: KeyTransformation, V: ClassManifest] extends CanBuildFrom[Coll, (K, Seq[V]), NavigableMultiMap[K, V]] {
    def apply() = newBuilder[K, V]

    def apply(from: Coll) = NavigableMultiMap.newBuilder[K, V]
  }
}
