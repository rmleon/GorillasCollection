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

// TODO: Implement transformation for string
//  implicit object StringKeyTransformation extends KeyTransformation[String] {
//    def transform(x: String) = {
//    }
//  }

}
