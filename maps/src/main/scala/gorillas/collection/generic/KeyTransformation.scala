package gorillas.collection.generic

/**
 * Ideally, a one to one linear transformation (preserving the ordering)
 * @tparam K Navigatable map key type
 */
trait KeyTransformation[K] {
  def transform(x: K): Int

  def apply(x: K): Int = transform(x)

}
