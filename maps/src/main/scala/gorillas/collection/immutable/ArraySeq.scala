package gorillas.collection.immutable

import collection.immutable.IndexedSeq
import collection.Iterator

/**
 * Array-backed seq.
 * The array is never modified.
 * @param elems input (the array should never be modified)
 * @param from starting index (inclusive)
 * @param to ending index (exclusive)
 * @author Ricardo Leon
 */
private[immutable] final class ArraySeq[A](elems: Array[A], from: Int, to: Int) extends IndexedSeq[A] with Immutable {
  assert(from <= to, "From index must be less or equal to index")
  assert(from < elems.length, "From index must be less than array length")
  assert(to <= elems.length, "To index must be less or equal the array length")

  def apply(idx: Int): A = {
    val actualIndex = from + idx
    if (actualIndex >= to || actualIndex < from)
      throw new IndexOutOfBoundsException("Invalid index")
    elems(idx)
  }

  def length = to - from

  override def iterator = new Iterator[A] {
    var idx = from - 1

    def hasNext: Boolean = idx + 1 < to

    def next(): A = {
      idx += 1
      elems(idx)
    }
  }
}
