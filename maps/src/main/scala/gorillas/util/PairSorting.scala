package gorillas.util

import util.Random
import compat.Platform
import annotation.tailrec

/**
 * This object contains modified sorting algorithms specialized for pairs of arrays where the first vector contains
 * the keys to be sorted and the second array containes the values.  The relative position of each pair of elements is
 * kept after doing the sorting.
 * @author Ricardo Leon
 */
object PairSorting {

  /**
   * Not stable, in-place sorting algorithm
   * @param ks keys (contents will be sorted)
   * @param vs values (contents will be sorted according to the keys ordering)
   * @tparam K keys type
   * @tparam V values type
   */
  def quickSort[K: Ordering, V](ks: Array[K], vs: Array[V]) {
    threeWayQuickSort[K, V](ks, vs, 0, ks.size - 1)
  }

  private[this] val rand = new Random

  /**
   * Based on the algorithm described on http://www.sorting-algorithms.com/quick-sort-3-way
   * Uses insertion sort for 5 or fewer elements.
   * @author Ricardo Leon
   * @param ks keys
   * @param vs values
   * @param low lowest point to sort (inclusive)
   * @param high highest point to sort (inclusive)
   * @param ordering for the keys
   * @tparam K key type
   * @tparam V value type
   */
  private[this] def threeWayQuickSort[K, V](ks: Array[K], vs: Array[V], low: Int, high: Int)(implicit ordering: Ordering[K]) {
    if (high - low < 6)
      insertionSort[K, V](ks, vs, low, high + 1)
    else {
      def swap(x: Int, y: Int) {
        val tempK = ks(y)
        val tempV = vs(y)
        ks(y) = ks(x)
        ks(x) = tempK
        vs(y) = vs(x)
        vs(x) = tempV
      }
      //assert(n < ks.size)
      // choose pivot (swap)
      // swap a[n,rand(1,n)]
      val tempIndex = rand.nextInt(high + 1 - low) + low // this seems to be working but it can be improved by selecting the median of the highest, middle, and lowest keys
      swap(high, tempIndex)
      // 3-way partition
      // i = 1, k = 1, p = n
      var i = low
      var k = low
      var p = high

      /*
      while i < p,
      if a[i] < a[n], swap a[i++,k++]
      else if a[i] == a[n], swap a[i,--p]
      else i++
      end
      */
      while (i < p) {
        if (ordering.lt(ks(i), ks(high))) {
          swap(i, k)
          i += 1
          k += 1
        } else if (ks(i).equals(ks(high))) {
          p -= 1
          swap(i, p)
        } else
          i += 1
      }

      // → invariant: a[p..n] all equal
      // → invariant: a[1..k-1] < a[p..n] < a[k..p-1]

      // move pivots to center
      // m = min(p-k,n-p+1)
      val m = scala.math.min(p - k, high - p + 1)
      // swap a[k..k+m-1,n-m+1..n]
      var ii = k
      var jj = high - m + 1
      while (ii <= k + m - 1) {
        swap(ii, jj)
        ii += 1
        jj += 1
      }
      //# recursive sorts
      //sort a[1..k-1]
      //sort a[n-p+k+1,n]
      threeWayQuickSort(ks, vs, low, k - 1)
      threeWayQuickSort(ks, vs, high - p + k + 1, high)
    }
  }

  /**
   * Stable reverse:
   * Reverses the array but keeps the original order for those values that have a repeated key.
   * @param ks keys array
   * @param vs values array (ks and vs should be the same size)
   * @tparam K key type
   * @tparam V value type
   */
  @inline private final def reverse[K, V](ks: Array[K], vs: Array[V]) {
    @tailrec def rev(x: Int, y: Int) {
      if (x < y) {
        val tempK = ks(x)
        val tempV = vs(x)
        ks(x) = ks(y)
        vs(x) = vs(y)
        ks(y) = tempK
        vs(y) = tempV
        rev(x + 1, y - 1)
      }
    }
    rev(0, ks.length - 1) // Reverse all the values
    var i = 0
    while (i < ks.length - 1) {
      // And then reverse the values that are under the same key
      var j = i + 1
      while (j < ks.length && ks(i) == ks(j)) {
        // Find rows of common keys
        j += 1
      }
      if (j > i + 1) {
        // j - 1 has the position of the last key that is the same
        rev(i, j - 1)
        i = j
      } else
        i += 1
    }
  }

  /**
   * Merge sort based on the algorithm described on http://www.sorting-algorithms.com/
   * Uses insertion sort for 5 or fewer elements
   * @param ks the keys
   * @param vs the values
   * @tparam K key type
   * @tparam V value type
   */
  def mergeSort[K: Ordering: ClassManifest, V: ClassManifest](ks: Array[K], vs: Array[V]) {
    val length = ks.length
    if (length < 6) // 6 seems to be the limit where insertion sort is faster than merge sort (including the checks below)
      insertionSort(ks, vs, 0, length)
    else {
      val comparison = direction(ks, length)
      if (comparison == -1)
        reverse(ks, vs)
      else if (comparison == 2)
        mergeSort(ks, vs, new Array[K](length), new Array[V](length), 0, length)
    }
  }

  /**
   * Merge sort based on the algorithm described on http://www.sorting-algorithms.com/
   * @author Ricardo Leon
   * @param ks keys
   * @param vs values
   * @param sk scratch array for keys
   * @param sv scratch array for values
   * @param low lowest index to sort (inclusive)
   * @param high highest index to sort (exclusive)
   * @param ordering ordering on K
   * @tparam K keys type
   * @tparam V values type
   */
  private[this] def mergeSort[K, V](ks: Array[K], vs: Array[V], sk: Array[K], sv: Array[V], low: Int, high: Int)(implicit ordering: Ordering[K]) {
    //if (low < high - 1) {
    //    # split in half
    //    m = n / 2
    val m = ((high - low) / 2) + low
    //
    //    # recursive sorts
    //      sort a[1..m]
    //    sort a[m+1..n]
    if (low < m - 1)
      mergeSort[K, V](ks, vs, sk, sv, low, m)
    if (m < high - 1)
      mergeSort[K, V](ks, vs, sk, sv, m, high)

    //
    //    # merge sorted sub-arrays using temp array
    //      b = copy of a[1..m]
    Platform.arraycopy(ks, low, sk, low, m - low)
    Platform.arraycopy(vs, low, sv, low, m - low)

    //    i = 1, j = m+1, k = 1
    var i = low
    var k = low
    var j = m

    //    while i <= m and j <= n,
    //    a[k++] = (a[j] < b[i]) ? a[j++] : b[i++]
    //    → invariant: a[1..k] in final position
    while (i < m && j < high) {
      if (ordering.lt(ks(j), sk(i))) {
        ks(k) = ks(j)
        vs(k) = vs(j)
        j += 1
      } else {
        ks(k) = sk(i)
        vs(k) = sv(i)
        i += 1
      }
      k += 1
    }

    //    while i <= m,
    //    a[k++] = b[i++]
    //    → invariant: a[1..k] in final position
    Platform.arraycopy(sk, i, ks, k, m - i)
    Platform.arraycopy(sv, i, vs, k, m - i)
    //      while (i < m) {
    //        ks(k) = sk(i)
    //        vs(k) = sv(i)
    //        k += 1
    //        i += 1
    //      }
    //}
  }

  def insertionSort[K: Ordering, V](ks: Array[K], vs: Array[V]) {
    insertionSort[K, V](ks, vs, 0, ks.length)
  }

  /**
   * Simple sort that is very fast for very small sizes.
   * Based on http://www.sorting-algorithms.com/insertion-sort
   * @author Ricardo Leon
   * @param ks keys
   * @param vs values
   * @param low lowest index to sort (inclusive)
   * @param high highest index to sort (exclusive)
   * @param ordering key ordering
   * @tparam K key type
   * @tparam V key value
   */
  private[this] final def insertionSort[K, V](ks: Array[K], vs: Array[V], low: Int, high: Int)(implicit ordering: Ordering[K]) {
    // for i = 2:n,
    var i = low + 1
    while (i < high) {
      // for (k = i; k > 1 and a[k] < a[k-1]; k--)
      var k = i
      while (k > 0 && ordering.lt(ks(k), ks(k - 1))) {
        //     swap a[k,k-1]
        val tempK = ks(k - 1)
        val tempV = vs(k - 1)
        ks(k - 1) = ks(k)
        vs(k - 1) = vs(k)
        ks(k) = tempK
        vs(k) = tempV
        k -= 1
      }
      i += 1
    }
  }

  /**
   * Checks whether an array has all its elements in ascending or descending ordering.
   * Assumes that the array has 2 or more elements
   * @return -1 if the array is descending.  0 if the array is all equals.  1 if the array is ascending.  2 if the array is not ordered.
   */
  protected[util] final def direction[A](array: Array[A], length: Int)(implicit ordering: Ordering[A]) = {
    var i = 1
    var initial = 0
    while (i < length && initial == 0) {
      initial = ordering.compare(array(i - 1), array(i))
      i += 1
    }
    initial *= -1

    while (i < length && initial != ordering.compare(array(i - 1), array(i)))
      i += 1

    if (i != length)
      2
    else
      initial
  }
}
