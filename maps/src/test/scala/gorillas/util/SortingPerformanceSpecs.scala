package gorillas.util

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import util.Random
import compat.Platform

/**
 * Non-deterministic tests used as a guideline to determine when I'm making progress.
 * TODO: Check other cases such as ascending, descending, few keys, and almost sorted.
 * @author Ricardo Leon
 */
class SortingPerformanceSpecs extends FunSpec with ShouldMatchers {
  def fixturesRandom(size: Int, baseArray: Array[Int]) = new {

    val random = new Random(6666)
    var i = 0
    while (i < size) {
      baseArray(i) = random.nextInt()
      i += 1
    }
    val dummyValuesArray = new Array[Int](size)
  }

  describe("Sorting.mergeSort") {
    it("should find the optimal size at which insertion sort is slower than merge sort") {
      val repetitions = 100000
      var firstDifference = 0
      for (i <- 1 until 20) {
        val baseArray = new Array[Int](i)
        val copyArray = new Array[Int](i)

        // Warm up:
        var j = 0
        while (j < 100) {
          var f = fixturesRandom(i, baseArray)
          PairSorting.mergeSort(baseArray, f.dummyValuesArray)
          f = fixturesRandom(i, baseArray)
          PairSorting.quickSort(baseArray, f.dummyValuesArray)
          f = fixturesRandom(i, baseArray)
          PairSorting.insertionSort(baseArray, f.dummyValuesArray)
          f = fixturesRandom(i, baseArray)
          Sorting.stableSort(baseArray, f.dummyValuesArray)
          j += 1
        }

        j = 0
        val startTime2 = System.currentTimeMillis()
        while (j < repetitions) {
          val f = fixturesRandom(i, baseArray)
          PairSorting.quickSort(baseArray, f.dummyValuesArray)
          j += 1
        }
        val endTime2 = System.currentTimeMillis()

        j = 0
        val startTime1 = System.currentTimeMillis()
        while (j < repetitions) {
          val f = fixturesRandom(i, baseArray)
          PairSorting.mergeSort(baseArray, f.dummyValuesArray)
          j += 1
        }
        val endTime1 = System.currentTimeMillis()

        j = 0
        val startTime3 = System.currentTimeMillis()
        while (j < repetitions) {
          val f = fixturesRandom(i, baseArray)
          PairSorting.insertionSort(baseArray, f.dummyValuesArray)
          j += 1
        }
        val endTime3 = System.currentTimeMillis()

        j = 0
        val startTime4 = System.currentTimeMillis()
        while (j < repetitions) {
          Platform.arraycopy(baseArray, 0, copyArray, 0, i)
          j += 1
        }
        val endTime4 = System.currentTimeMillis()

        j = 0
        val startTime5 = System.currentTimeMillis()
        while (j < repetitions) {
          var k = 0
          while (k < i) {
            copyArray(k) = baseArray(k)
            k += 1
          }
          j += 1
        }
        val endTime5 = System.currentTimeMillis()

        j = 0
        val startTime6 = System.currentTimeMillis()
        while (j < repetitions) {
          val f = fixturesRandom(i, baseArray)
          Sorting.stableSort(baseArray, f.dummyValuesArray)
          j += 1
        }
        val endTime6 = System.currentTimeMillis()


        println("Length: %d\tMS: %d\tQS: %d\tIS: %d, AC: %d, WC: %d, SS: %d".format(i, endTime1 - startTime1, endTime2 - startTime2, endTime3 - startTime3, endTime4 - startTime4, endTime5 - startTime5, endTime6 - startTime6))
      }

    }
  }


}