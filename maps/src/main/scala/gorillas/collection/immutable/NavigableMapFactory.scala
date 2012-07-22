package gorillas.collection.immutable

import gorillas.collection.mutable.NavigableMapBuilder
import gorillas.collection.generic.KeyTransformation

/**
 * @author Ricardo Leon
 */
abstract class NavigableMapFactory {

  def empty[K, V](implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMap[K, V] =
    new NavigableMap0[K, V]

  def apply[K, V](elements: (K, V)*)
                 (implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMap[K, V] =
    (newBuilder[K, V] ++= elements).result()

  def newBuilder[K, V](implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMapBuilder[K, V] =
    new NavigableMapBuilder[K, V]

  private[collection] def single[K, V](singleEntry: (K, V))
                                      (implicit ord: Ordering[K], key2int: KeyTransformation[K], keyManifest: ClassManifest[K], valueManifest: ClassManifest[V]): NavigableMap[K, V] =
    new NavigableMap1[K, V](singleEntry._1, singleEntry._2)
}
