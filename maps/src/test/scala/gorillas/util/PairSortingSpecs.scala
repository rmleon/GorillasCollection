package gorillas.util

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import collection.mutable.ArrayBuffer

/**
 * Sorting
 * @author Ricardo Leon
 */
class PairSortingSpecs extends FunSpec with ShouldMatchers {

  describe("Sorting.quickSort") {
    it("should return a sorted buffer as it is originally") {
      val keys = ArrayBuffer(3, 5, 7).toArray
      val values = ArrayBuffer(4, 6, 8).toArray
      val expectedKeys = keys.clone()
      val expectedValues = values.clone()
      PairSorting.quickSort(keys, values)
      expectedValues should equal(values)
      expectedKeys should equal(keys)
    }

    it("should sort a reversed buffer") {
      val keys = ArrayBuffer(7, 5, 3).toArray
      val values = ArrayBuffer(8, 6, 4).toArray
      PairSorting.quickSort(keys, values)
      (3 :: 5 :: 7 :: Nil).toArray should equal(keys)
      (4 :: 6 :: 8 :: Nil).toArray should equal(values)
    }

    it("should sort a nearly sorted buffer") {
      val keys = ArrayBuffer(1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11, 13, 14, 12, 15, 16, 17, 18).toArray
      val values = keys.map(_ + 20)
      PairSorting.quickSort(keys, values)
      val expectedKeys = Range.inclusive(1, 18)
      val expectedValues = expectedKeys.map(_ + 20)
      expectedKeys.toArray should equal(keys)
      expectedValues.toArray should equal(values)
    }
  }

  describe("Sorting.mergeSort") {
    it("should return a sorted buffer as it is originally") {
      val keys = ArrayBuffer(3, 5, 7).toArray
      val values = ArrayBuffer(4, 6, 8).toArray
      val expectedKeys = keys.clone()
      val expectedValues = values.clone()
      PairSorting.mergeSort(keys, values)
      expectedValues should equal(values)
      expectedKeys should equal(keys)
    }

    it("should sort a reversed buffer") {
      val keys = ArrayBuffer(7, 5, 3, 1).toArray
      val values = ArrayBuffer(8, 6, 4, 2).toArray
      PairSorting.mergeSort(keys, values)
      (1 :: 3 :: 5 :: 7 :: Nil).toArray should equal(keys)
      (2 :: 4 :: 6 :: 8 :: Nil).toArray should equal(values)
    }

    it("should sort a nearly sorted buffer") {
      val keys = ArrayBuffer(1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11, 13, 14, 12, 15, 16, 17, 18).toArray
      val values = keys.map(_ + 20)
      PairSorting.mergeSort(keys, values)
      val expectedKeys = Range.inclusive(1, 18).toArray
      val expectedValues = expectedKeys.map(_ + 20)
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the array is ascending") {
      val keys = Range(1, 20).toArray
      val values = Range(20, 1).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = Range(1, 20).toArray
      val expectedValues = Range(20, 1).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the array is the same") {
      val keys = List(3, 3, 3, 3, 3, 3, 3).toArray
      val values = List(7, 6, 5, 4, 3, 2, 1).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = List(3, 3, 3, 3, 3, 3, 3).toArray
      val expectedValues = List(7, 6, 5, 4, 3, 2, 1).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the keys array has some repeated elements (reversed, less than 6 elements)") {
      val keys = List(3, 2, 2, 1, 0).toArray
      val values = List(4, 3, 2, 1, 0).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = List(0, 1, 2, 2, 3).toArray
      val expectedValues = List(0, 1, 3, 2, 4).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the keys array has some repeated elements (not ordered, less than 6 elements)") {
      val keys = List(4, 5, 2, 2, 6).toArray
      val values = List(5, 4, 3, 2, 1).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = List(2, 2, 4, 5, 6).toArray
      val expectedValues = List(3, 2, 5, 4, 1).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the keys array has some repeated elements (reversed, 10 elements)") {
      val keys = List(10, 9, 8, 7, 6, 5, 3, 2, 2, 1, 0).toArray
      val values = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = List(0, 1, 2, 2, 3, 5, 6, 7, 8, 9, 10).toArray
      val expectedValues = List(0, 1, 3, 2, 4, 5, 6, 7, 8, 9, 10).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }

    it("should maintain the values order when the keys array has some repeated elements (not ordered, 10 elements)") {
      val keys = List(4, 5, 2, 2, 6, 10, 20, 30, 40, 50).toArray
      val values = List(5, 4, 3, 2, 1, 10, 20, 30, 40, 50).toArray
      PairSorting.mergeSort(keys, values)
      val expectedKeys = List(2, 2, 4, 5, 6, 10, 20, 30, 40, 50).toArray
      val expectedValues = List(3, 2, 5, 4, 1, 10, 20, 30, 40, 50).toArray
      expectedKeys should equal(keys)
      expectedValues should equal(values)
    }
  }

  describe("Sorting.insertionSort") {
    it("should return a sorted buffer as it is originally") {
      val keys = ArrayBuffer(3, 5, 7).toArray
      val values = ArrayBuffer(4, 6, 8).toArray
      val expectedKeys = keys.clone()
      val expectedValues = values.clone()
      PairSorting.insertionSort(keys, values)
      expectedValues should equal(values)
      expectedKeys should equal(keys)
    }

    it("should sort a reversed buffer") {
      val keys = ArrayBuffer(7, 5, 3).toArray
      val values = ArrayBuffer(8, 6, 4).toArray
      PairSorting.insertionSort(keys, values)
      (3 :: 5 :: 7 :: Nil).toArray should equal(keys)
      (4 :: 6 :: 8 :: Nil).toArray should equal(values)
    }

    it("should sort a nearly sorted buffer") {
      val keys = ArrayBuffer(1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11, 13, 14, 12, 15, 16, 17, 18).toArray
      val values = keys.map(_ + 20)
      PairSorting.insertionSort(keys, values)
      val expectedKeys = Range.inclusive(1, 18)
      val expectedValues = expectedKeys.map(_ + 20)
      expectedKeys.toArray should equal(keys)
      expectedValues.toArray should equal(values)
    }
  }

  describe("Sorting.direction") {
    it("should return -1 when the array is descending") {
      val values = ArrayBuffer(5, 4, 3, 2, 1)
      PairSorting.direction(values.toArray, values.size) should equal(-1)
    }

    it("should return 1 when the array is ascending") {
      val values = ArrayBuffer(1, 2, 3, 4, 5)
      PairSorting.direction(values.toArray, values.size) should equal(1)
    }

    it("should return 1 when the array contains the same element") {
      val values = ArrayBuffer(3, 3, 3, 3, 3)
      PairSorting.direction(values.toArray, values.size) should equal(1)
    }

    it("should return 0 when the array contains different elements") {
      val values = ArrayBuffer(3, 3, 3, 6, 3, 3)
      PairSorting.direction(values.toArray, values.size) should equal(0)
    }
  }

}
