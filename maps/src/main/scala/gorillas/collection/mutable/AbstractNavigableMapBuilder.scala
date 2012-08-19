package gorillas.collection.mutable

import compat.Platform
import collection.{ IndexedSeqLike, GenTraversableOnce }

private[mutable] abstract class AbstractNavigableMapBuilder[K: ClassManifest, V: ClassManifest] {

  /**
   * Specialized container used to hold the keys and values.
   * It gives me full access to the array to avoid unnecessary data duplication, allow quick access to the sorting, and a memory-light implementation.
   * @tparam A type contained in the array.
   */
  protected final class ResizableArray[A: ClassManifest] {
    var array = new Array[A](16) // Unlike Scala's ResizableArray and ArrayBuffer, this allows me direct access to the backing array

    var size = 0

    @inline final def apply(i: Int) = if (i >= size) throw new ArrayIndexOutOfBoundsException(i) else array(i)

    def sizeHint(newSize: Int) {
      ensureSize(newSize)
    }

    def ensureSize(newSize: Int) {
      if (newSize > array.length) {
        val newArray = new Array[A](newSize) // Unlike the default implementation, I'm not going to try to increase the array more than it is needed if I get a hint.  Block allocation might not match evenly, but I'll take the risk.
        Platform.arraycopy(array, 0, newArray, 0, size)
        array = newArray
      }
    }

    private[this] def growSize(additional: Int) {
      if (size + additional > array.length) {
        var newSize = array.length * 2
        while (newSize < size + additional) newSize *= 2
        ensureSize(newSize)
      }
    }

    def clear() {
      size = 0
      array = new Array[A](16)
    }

    def +=(e: A) = {
      growSize(1)
      array(size) = e
      size += 1
      this
    }

    def ++=[A1 <: A](source: Array[A1]): ResizableArray[A] = ++=(source, 0, source.length)

    def ++=[A1 <: A](source: Array[A1], sourceStart: Int, sourceLen: Int) = {
      growSize(sourceLen)
      Platform.arraycopy(source, sourceStart, array, size, sourceLen)
      size += sourceLen
      this
    }

    def ++=[A1 <: A](source: Seq[A1]) = {

    }

    def toArray: Array[A] = {
      val result = new Array[A](size)
      Platform.arraycopy(array, 0, result, 0, size)
      result
    }

  }

  protected val keys = new ResizableArray[K]

  protected val values = new ResizableArray[V]

  def sizeHint(size: Int)

  def ++=[V1 <: V](ks: Array[K], vs: Array[V1], sourceStart: Int, sourceLen: Int): this.type = {
    require(ks.length == vs.length, "Keys and values must have the same size")
    require(sourceStart + sourceLen <= ks.length, "sourceStart + sourceLen must be less or equal the arrays length")
    keys ++= (ks, sourceStart, sourceLen)
    values ++= (vs, sourceStart, sourceLen)
    this
  }

  def +=(x: (K, V)): this.type = {
    keys += x._1
    values += x._2
    this
  }

  def ++=(xs: GenTraversableOnce[(K, V)]): this.type = {
    if (xs.isInstanceOf[IndexedSeqLike[_, _]])
      sizeHint(xs.size + keys.size + 1)
    xs.foreach {
      entry =>
        keys += entry._1
        values += entry._2
    }
    this
  }

  def ++=[V1 <: V](ks: Array[K], vs: Array[V1]): this.type = {
    require(ks.length == vs.length, "Keys and values must have the same size")
    sizeHint(ks.length + keys.size + 1)
    keys ++= ks
    values ++= vs
    this
  }
}
