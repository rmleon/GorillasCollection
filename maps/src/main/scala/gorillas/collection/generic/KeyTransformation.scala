package gorillas.collection.generic

/**
 * Ideally, a one to one linear transformation (preserving the ordering)
 * @tparam K Navigatable map key type
 */
trait KeyTransformation[K] {
  def transform(x: K): Int

  def apply(x: K): Int = transform(x)

}

/**
 * Implicits for common types
 */
object KeyTransformation {
  implicit object IntKeyTransformation extends KeyTransformation[Int] {
    def transform(x: Int) = x
  }

  implicit object FloatKeyTransformation extends KeyTransformation[Float] {
    final val divisor = Float.MaxValue / Int.MaxValue.toDouble
    def transform(x: Float) = (x / divisor).toInt
  }

  implicit object DoubleKeyTransformation extends KeyTransformation[Double] {
    final val divisor = Double.MaxValue / Int.MaxValue.toDouble
    def transform(x: Double) = (x / divisor).toInt
  }

  implicit object LongKeyTransformation extends KeyTransformation[Long] {
    final val divisor = Long.MaxValue / Int.MaxValue.toLong
    def transform(x: Long) = (x / divisor).toInt
  }

  /**
   * This assumes that String Ordering follow's the natural ordering.
   * If this is not the case, a transformation that yields integers
   */
  implicit object StringKeyTransformation extends KeyTransformation[String] {
    /**
     * Assumes that the Unicode's code point uses 2 bytes at most.
     */
    def transform(x: String) = {
      x.length match {
        case 0 => 0
        case 1 => x.codePointAt(0) << 15 // Using 15 and dropping the last bit of the second characters avoid negative values
        case _ => (x.codePointAt(0) << 15) | (x.codePointAt(1) >> 1)
      }
    }
  }
}