package gorillas.util

/**
SCALA LICENSE

Copyright (c) 2002-2012 EPFL, Lausanne, unless otherwise specified.
All rights reserved.

This software was developed by the Programming Methods Laboratory of the
Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.

Permission to use, copy, modify, and distribute this software in source
or binary form for any purpose with or without fee is hereby granted,
provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   3. Neither the name of the EPFL nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
 */

/**
 * This implementation is based on Scala's Sorting object.
 * It is modified to work with two arrays that represent a key and a value (instead of single keys array).
 * The original Scala license is kept for this file.
 */
object Sorting {
  /**
   * Special stable sort implementation that returns the results as arrays.
   * Each time an element is sorted in ks, the corresponding element in vs will be sorted as well.
   * @param ks these are the unordered keys.  This sequence will be sorted after this is done.
   * @param vs these are the values.  The index and size should match with ks.  This sequence will be sorted according to ks ordering.
   * @tparam K keys type
   * @tparam V values type
   * @return a tuple of two arrays with the results
   */
  def stableSort[K: Ordering : ClassManifest, V: ClassManifest](ks: Array[K], vs: Array[V]): (Array[K], Array[V]) = {
    val scratchK = new Array[K](ks.length)
    val scratchV = new Array[V](ks.length)
    stableSort[K, V](ks, vs, 0, ks.length - 1, scratchK, scratchV)
    (scratchK -> scratchV)
  }

  /**
   * Copied from scala.util.Sorting.
   * Modified to take an array representing the keys and another array containing the corresponding values.
   * @author  Ross Judson
   * @author Ricardo Leon
   */
  private[this] def stableSort[K, V](ks: Array[K], vs: Array[V], lo: Int, hi: Int, scratchK: Array[K], scratchV: Array[V])
                                    (implicit ordering: Ordering[K]) {
    if (lo < hi) {
      val mid = (lo + hi) >> 1
      stableSort[K, V](ks, vs, lo, mid, scratchK, scratchV)
      stableSort[K, V](ks, vs, mid + 1, hi, scratchK, scratchV)
      var k, t_lo = lo
      var t_hi = mid + 1
      while (k <= hi) {
        if ((t_lo <= mid) && ((t_hi > hi) || (!ordering.lt(ks(t_hi), ks(t_lo))))) {
          scratchK(k) = ks(t_lo)
          scratchV(k) = vs(t_lo)
          t_lo += 1
        } else {
          scratchK(k) = ks(t_hi)
          scratchV(k) = vs(t_hi)
          t_hi += 1
        }
        k += 1
      }
      k = lo
      while (k <= hi) {
        ks(k) = scratchK(k)
        vs(k) = scratchV(k)
        k += 1
      }
    }
  }
}
