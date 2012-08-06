package gorillas.collection.immutable

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.ArrayBuffer
import util.Random
import gorillas.collection.generic.KeyTransformation
import collection.SortedMap

/**
 * @author Ricardo Leon
 */

class NavigableMapSpec extends FlatSpec with ShouldMatchers {

  type Key = Int

  type Value = String

  val testValues: Seq[(Key, Value)] =
    Seq(
      (1 -> "One"),
      (2 -> "Two"),
      (3 -> "Three"),
      (4 -> "Four"),
      (5 -> "Five"),
      (6 -> "Six"))

  implicit val trans = new KeyTransformation[Key] {
    def transform(x: Key) = x
  }

  val navigableMapWith3Entries = NavigableMap((1 -> "One"), (3 -> "Three"), (5 -> "Five"))

  val singleMap = NavigableMap(3 -> "Three")

  "Empty NavigableMap" should "not return a value" in {
    val map = NavigableMap.empty[Key, Value]
    expect(0)(map.size)
    expect(None)(map.get(1))
  }

  "NavigableMap" should "return all 2 objects" in testNResults(2)
  it should "return all 3 objects" in testNResults(3)
  it should "return all 4 objects" in testNResults(4)
  it should "return all 5 objects" in testNResults(5)
  it should "return all %d objects".format(testValues.length) in testNResults(6)
  it should "handle key misses" in {
    val builder = NavigableMap.newBuilder[Key, Value]
    builder ++= testValues
    val map = builder result ()
    assert(map.size == testValues.length)
    (20 until 100).foreach {
      index =>
        expect(None) {
          map.get(index)
        }
    }
  }
  it should "return intermittent values" in {
    expect(None)(navigableMapWith3Entries.get(0))
    expect("One")(navigableMapWith3Entries.get(1).get)
    expect(None)(navigableMapWith3Entries.get(2))
    expect("Three")(navigableMapWith3Entries.get(3).get)
    expect(None)(navigableMapWith3Entries.get(4))
    expect("Five")(navigableMapWith3Entries.get(5).get)
    expect(None)(navigableMapWith3Entries.get(6))
    expect(None)(navigableMapWith3Entries.get(7))
  }
  "NavigableMap.floorKey" should "work" in {
    expect(None)(navigableMapWith3Entries.floorKey(-1000))
    expect(None)(navigableMapWith3Entries.floorKey(-1))
    expect(None)(navigableMapWith3Entries.floorKey(0))
    expect(Some(1))(navigableMapWith3Entries.floorKey(1))
    expect(Some(1))(navigableMapWith3Entries.floorKey(2))
    expect(Some(3))(navigableMapWith3Entries.floorKey(3))
    expect(Some(3))(navigableMapWith3Entries.floorKey(4))
    expect(Some(5))(navigableMapWith3Entries.floorKey(5))
    expect(Some(5))(navigableMapWith3Entries.floorKey(6))
    expect(Some(5))(navigableMapWith3Entries.floorKey(7))
    expect(Some(5))(navigableMapWith3Entries.floorKey(1000))
  }
  "NavigableMap.lowerKey" should "work" in {
    expect(None)(navigableMapWith3Entries.lowerKey(-1000))
    expect(None)(navigableMapWith3Entries.lowerKey(-1))
    expect(None)(navigableMapWith3Entries.lowerKey(0))
    expect(None)(navigableMapWith3Entries.lowerKey(1))
    expect(Some(1))(navigableMapWith3Entries.lowerKey(2))
    expect(Some(1))(navigableMapWith3Entries.lowerKey(3))
    expect(Some(3))(navigableMapWith3Entries.lowerKey(4))
    expect(Some(3))(navigableMapWith3Entries.lowerKey(5))
    expect(Some(5))(navigableMapWith3Entries.lowerKey(6))
    expect(Some(5))(navigableMapWith3Entries.lowerKey(7))
    expect(Some(5))(navigableMapWith3Entries.lowerKey(8))
    expect(Some(5))(navigableMapWith3Entries.lowerKey(1000))
  }
  "NavigableMap.ceilingKey" should "work" in {
    expect(Some(1))(navigableMapWith3Entries.ceilingKey(-1000))
    expect(Some(1))(navigableMapWith3Entries.ceilingKey(-1))
    expect(Some(1))(navigableMapWith3Entries.ceilingKey(0))
    expect(Some(1))(navigableMapWith3Entries.ceilingKey(1))
    expect(Some(3))(navigableMapWith3Entries.ceilingKey(2))
    expect(Some(3))(navigableMapWith3Entries.ceilingKey(3))
    expect(Some(5))(navigableMapWith3Entries.ceilingKey(4))
    expect(Some(5))(navigableMapWith3Entries.ceilingKey(5))
    expect(None)(navigableMapWith3Entries.ceilingKey(6))
    expect(None)(navigableMapWith3Entries.ceilingKey(7))
    expect(None)(navigableMapWith3Entries.ceilingKey(1000))
  }
  "NavigableMap.higherKey" should "work" in {
    expect(Some(1))(navigableMapWith3Entries.higherKey(-1000))
    expect(Some(1))(navigableMapWith3Entries.higherKey(-1))
    expect(Some(1))(navigableMapWith3Entries.higherKey(0))
    expect(Some(3))(navigableMapWith3Entries.higherKey(1))
    expect(Some(3))(navigableMapWith3Entries.higherKey(2))
    expect(Some(5))(navigableMapWith3Entries.higherKey(3))
    expect(Some(5))(navigableMapWith3Entries.higherKey(4))
    expect(None)(navigableMapWith3Entries.higherKey(5))
    expect(None)(navigableMapWith3Entries.higherKey(6))
    expect(None)(navigableMapWith3Entries.higherKey(7))
    expect(None)(navigableMapWith3Entries.higherKey(1000))
  }

  "SingleNavigableMap.floorKey" should "work" in {
    expect(None)(singleMap.floorKey(-1))
    expect(None)(singleMap.floorKey(0))
    expect(None)(singleMap.floorKey(1))
    expect(None)(singleMap.floorKey(2))
    expect(Some(3))(singleMap.floorKey(3))
    expect(Some(3))(singleMap.floorKey(4))
    expect(Some(3))(singleMap.floorKey(5))
  }

  "SingleNavigableMap.ceilingKey" should "work" in {
    expect(Some(3))(singleMap.ceilingKey(-1))
    expect(Some(3))(singleMap.ceilingKey(0))
    expect(Some(3))(singleMap.ceilingKey(1))
    expect(Some(3))(singleMap.ceilingKey(2))
    expect(Some(3))(singleMap.ceilingKey(3))
    expect(None)(singleMap.ceilingKey(4))
    expect(None)(singleMap.ceilingKey(5))
    expect(None)(singleMap.ceilingKey(6))
    expect(None)(singleMap.ceilingKey(7))
  }
  "SingleNavigableMap.lowerKey" should "work" in {
    expect(None)(singleMap.lowerKey(-1000))
    expect(None)(singleMap.lowerKey(-1))
    expect(None)(singleMap.lowerKey(0))
    expect(None)(singleMap.lowerKey(1))
    expect(None)(singleMap.lowerKey(2))
    expect(None)(singleMap.lowerKey(3))
    expect(Some(3))(singleMap.lowerKey(4))
    expect(Some(3))(singleMap.lowerKey(5))
    expect(Some(3))(singleMap.lowerKey(1000))
  }
  "SingleNavigableMap.higherKey" should "work" in {
    expect(Some(3))(singleMap.higherKey(-1000))
    expect(Some(3))(singleMap.higherKey(-1))
    expect(Some(3))(singleMap.higherKey(0))
    expect(Some(3))(singleMap.higherKey(1))
    expect(Some(3))(singleMap.higherKey(2))
    expect(None)(singleMap.higherKey(3))
    expect(None)(singleMap.higherKey(4))
    expect(None)(singleMap.higherKey(5))
    expect(None)(singleMap.higherKey(1000))
  }
  "NavigableMap.rangeImpl" should "work" in {
    val all = navigableMapWith3Entries.rangeImpl(Some(0), Some(6))
    all should equal(navigableMapWith3Entries)
    expect(all.size)(navigableMapWith3Entries.size)
    expect("One")(all(1))
    expect("Three")(all(3))
    expect("Five")(all(5))

    val all2 = navigableMapWith3Entries.rangeImpl(None, None)
    expect(navigableMapWith3Entries)(all2)

    val all3 = navigableMapWith3Entries.rangeImpl(None, Some(6))
    expect(navigableMapWith3Entries)(all3)

    expect(NavigableMap.empty[Int, String])(navigableMapWith3Entries.rangeImpl(None, Some(0)))

    val all4 = navigableMapWith3Entries.rangeImpl(Some(-10000), None)
    expect(navigableMapWith3Entries)(all4)

    expect(NavigableMap.empty[Int, String])(navigableMapWith3Entries.rangeImpl(Some(10000), None))

    val twoLowerElements = navigableMapWith3Entries.rangeImpl(Some(1), Some(4))
    expect(2)(twoLowerElements.size)
    expect("One")(twoLowerElements(1))
    expect("Three")(twoLowerElements(3))

    val twoHigherElements = navigableMapWith3Entries.rangeImpl(Some(3), Some(6))
    expect(2)(twoLowerElements.size)
    expect("Five")(twoHigherElements(5))
    expect("Three")(twoHigherElements(3))
    expect(None)(twoHigherElements.get(1))

    // Test upped bound
    val onlyOne = navigableMapWith3Entries.rangeImpl(Some(1), Some(3))
    expect(1)(onlyOne.size)
    expect(None)(onlyOne.get(5))
    expect(None)(onlyOne.get(3))
    expect("One")(onlyOne(1))

    // Test empty
    val emptyMap = navigableMapWith3Entries.rangeImpl(Some(3), Some(3))
    expect(0)(emptyMap.size)
    expect(None)(emptyMap.get(5))
    expect(None)(emptyMap.get(3))
    expect(None)(emptyMap.get(1))

    // Test empty
    val emptyMap2 = navigableMapWith3Entries.rangeImpl(Some(4), Some(2))
    expect(0)(emptyMap2.size)
    expect(None)(emptyMap2.get(5))
    expect(None)(emptyMap2.get(3))
    expect(None)(emptyMap2.get(1))

    // Test empty
    val emptyMap3 = navigableMapWith3Entries.rangeImpl(Some(7), Some(10000))
    expect(NavigableMap.empty[Int, String])(emptyMap3)

    // Test empty
    val emptyMap4 = navigableMapWith3Entries.rangeImpl(Some(-10000), Some(0))
    expect(NavigableMap.empty[Int, String])(emptyMap4)

    val emptyMap5 = navigableMapWith3Entries.rangeImpl(Some(10000), Some(-10000))
    expect(NavigableMap.empty[Int, String])(emptyMap5)

  }
  "SingleNavigableMap" should "returns only the last values for each key" in {
    val builder = NavigableMap.newBuilder[Key, Value]
    builder += ((1, "One"))
    val map = builder result ()
    expect(1) {
      map.size
    }
    val result = map.get(1)
    assert(!result.isEmpty)
    expect("One") {
      result.get
    }
  }

  "NavigableMap.values" should "returns only the last values for each key" in {
    val map = NavigableMap(1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect("ONE" :: "Two" :: Nil) {
      map.values.toList
    }
    val map2 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect("Zero" :: "ONE" :: "Two" :: Nil) {
      map2.values.toList
    }
    val map3 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE")
    expect("Zero" :: "ONE" :: Nil) {
      map3.values.toList
    }
  }

  "NavigableMap.valuesIterator" should "returns only the last values for each key" in {
    val map = NavigableMap(1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect("ONE" :: "Two" :: Nil) {
      map.valuesIterator.toList
    }
    val map2 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect("Zero" :: "ONE" :: "Two" :: Nil) {
      map2.valuesIterator.toList
    }
    val map3 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE")
    expect("Zero" :: "ONE" :: Nil) {
      map3.valuesIterator.toList
    }
  }

  "NavigableMap.keys" should "returns the correct set" in {
    val map = NavigableMap(1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect(1 :: 2 :: Nil) {
      map.keys.toList
    }
    val map2 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE", 2 -> "Two")
    expect(0 :: 1 :: 2 :: Nil) {
      map2.keys.toList
    }
    val map3 = NavigableMap(0 -> "Zero", 1 -> "One", 1 -> "ONE")
    expect(0 :: 1 :: Nil) {
      map3.keys.toList
    }
  }

  "NavigableMap" should "return all 1,000,000 (random) objects" in {
    testWithRandomValues(1000000, 67, 1)
  }

  it should "handle duplicate values" in {
    val map1 = NavigableMap(1 -> "One", 2 -> "Two", 1 -> "ONE")
    val rightAnswer = NavigableMap(1 -> "ONE", 2 -> "Two")
    expect(rightAnswer)(map1)
  }

  "NavigableMap.+" should "work" in {
    val baseMap = NavigableMap(3 -> "Three")
    val map1 = baseMap + (1 -> "One")
    val map2 = map1 + (3 -> "THREE")
    expect(2)(map2.size)
    expect(2)(map1.size)
    expect("One")(map1(1))
    expect("Three")(map1(3))
    expect("THREE")(map2(3))
    expect("One")(map2(1))

    val emptyMap = NavigableMap.empty[Int, String]
    val fromEmpty = emptyMap + (7 -> "Seven")
    expect(1)(fromEmpty.size)
    expect("Seven")(fromEmpty(7))
  }

  "NavigableMap.+" should "work when appending elements" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three")
    val map1 = baseMap + (4 -> "Four")
    expect(SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four"))(map1)
  }

  "NavigableMap.+" should "work when prepending elements" in {
    val baseMap = NavigableMap(2 -> "Two", 3 -> "Three", 4 -> "Four")
    val map1 = baseMap + (1 -> "One")
    expect(SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four"))(map1)
  }

  "NavigableMap.+" should "work when inserting elements" in {
    val baseMap = NavigableMap(1 -> "One", 3 -> "Three", 4 -> "Four")
    val map1 = baseMap + (2 -> "Two")
    expect(SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four"))(map1)
  }

  "NavigableMap.+" should "work when inserting elements with duplicate keys" in {
    val baseMap = NavigableMap(1 -> "One", 3 -> "A", 3 -> "B", 3 -> "C", 3 -> "D", 4 -> "Four")
    val map1 = baseMap + (3 -> "E")
    expect(SortedMap(1 -> "One", 3 -> "A", 3 -> "B", 3 -> "C", 3 -> "D", 3 -> "E", 4 -> "Four"))(map1)
  }

  it should "handle duplicate values" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two")
    val duplicatedMap = baseMap + (1 -> "ONE")
    expect(2)(duplicatedMap.size)
    expect("ONE")(duplicatedMap(1))
  }
  it should "handle subtypes" in {
    class Test1(value1: Int)
    class Test2(val value1: Int, val value2: String) extends Test1(value1)
    val value1 = new Test1(1)
    val value2 = new Test2(2, "Two")
    val baseMap = NavigableMap(1 -> value1, 3 -> value1)
    val resultMap = baseMap + (2 -> value2)
    expect(3)(resultMap.size)
    expect(value1)(resultMap(1))
    expect(value2)(resultMap(2))

    val baseMap2 = NavigableMap(2 -> value2, 3 -> value2)
    val resultMap2 = baseMap2 + (1 -> value1)
    expect(3)(resultMap2.size)
    expect(value1)(resultMap2(1))
    expect(value2)(resultMap2(2))
  }

  "NavigableMap.-" should "work" in {
    val baseMap = NavigableMap((1 -> "One"), (3 -> "Three"), (5 -> "Five"))
    val map1 = baseMap - 1
    val map2 = map1 - 5
    val map3 = map2 - 0
    val emptyMap = map2 - 3

    expect("One")(baseMap(1))
    expect("Three")(baseMap(3))
    expect("Five")(baseMap(5))

    expect(None)(map1.get(1))
    expect("Three")(map1(3))
    expect("Five")(map1(5))

    expect(None)(map2.get(1))
    expect("Three")(map2(3))
    expect(None)(map2.get(5))

    expect(None)(map3.get(1))
    expect("Three")(map3(3))
    expect(None)(map3.get(5))

    expect(None)(emptyMap.get(1))
    expect(None)(emptyMap.get(3))
    expect(None)(emptyMap.get(5))
  }

  "NavigableMap.equals" should "use the maps's contents to determine equality" in {
    val map3_1 = NavigableMap((1 -> "One"), (3 -> "Three"), (5 -> "Five"))
    val map3_2 = NavigableMap((1 -> "One"), (3 -> "Three"), (5 -> "Five"))
    val map3_3 = NavigableMap((1 -> "One"), (3 -> "Three"), (5 -> "FIVE"))
    val map1_1 = NavigableMap((3 -> "Three"))
    val map1_2 = NavigableMap((3 -> "Three"))
    val map1_3 = NavigableMap((3 -> "THREE"))
    val empty1 = NavigableMap.empty[Int, String]
    val empty2 = NavigableMap.empty[Int, String]
    expect(true)(map3_1.equals(map3_2))
    expect(true)(map1_1.equals(map1_2))
    expect(true)(empty1.equals(empty2))

    expect(false)(map3_1.equals(map3_3))
    expect(false)(map1_1.equals(map1_3))
    expect(false)(map1_1.equals(empty1))
    expect(false)(empty1.equals(map3_3))
  }

  "NavigableMapBuilder" should "not duplicate values with the same key" in {
    val builder1 = NavigableMap.newBuilder[Int, Value]
    builder1 ++= Seq(1 -> "One", 2 -> "Two", 1 -> "ONE")
    val map1 = builder1.result()
    val rightAnswer = NavigableMap(1 -> "ONE", 2 -> "Two")
    expect(rightAnswer)(map1)
  }

  "NavigableMap.equals" should "return true for a sorted map with the same elements" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    expect(baseSortedMap)(baseMap)
  }

  "NavigableMap.filterKeys" should "work" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 1
    val nm1 = baseMap.filterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
  }

  "NavigableMap.withFilterKeys" should "work for size and, iterators, and keys" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 1
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
  }

  it should "work for contains, get, ceilingKey, higherKey, lowerKey, floorKey" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 1
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
    expect(sm1.get(1))(nm1.get(1))
    expect(sm1.get(2))(nm1.get(2))
    expect(sm1.get(3))(nm1.get(3))
    expect(sm1.contains(1))(nm1.contains(1))
    expect(sm1.contains(2))(nm1.contains(2))
    expect(sm1.contains(3))(nm1.contains(3))

    val nm2 = NavigableMap(2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    filterTests1(nm1, nm2)
  }

  def filterTests1[V](nm1: NavigableMap[Int,V], nm2: NavigableMap[Int,V]) {
    expect(nm2.ceilingKey(0))(nm1.ceilingKey(0))
    expect(nm2.ceilingKey(1))(nm1.ceilingKey(1))
    expect(nm2.ceilingKey(2))(nm1.ceilingKey(2))
    expect(nm2.ceilingKey(3))(nm1.ceilingKey(3))
    expect(nm2.ceilingKey(4))(nm1.ceilingKey(4))
    expect(nm2.ceilingKey(6))(nm1.ceilingKey(6))

    expect(nm2.higherKey(0))(nm1.higherKey(0))
    expect(nm2.higherKey(1))(nm1.higherKey(1))
    expect(nm2.higherKey(2))(nm1.higherKey(2))
    expect(nm2.higherKey(3))(nm1.higherKey(3))
    expect(nm2.higherKey(4))(nm1.higherKey(4))
    expect(nm2.higherKey(6))(nm1.higherKey(6))

    expect(nm2.lowerKey(0))(nm1.lowerKey(0))
    expect(nm2.lowerKey(1))(nm1.lowerKey(1))
    expect(nm2.lowerKey(2))(nm1.lowerKey(2))
    expect(nm2.lowerKey(3))(nm1.lowerKey(3))
    expect(nm2.lowerKey(4))(nm1.lowerKey(4))
    expect(nm2.lowerKey(6))(nm1.lowerKey(6))

    expect(nm2.floorKey(0))(nm1.floorKey(0))
    expect(nm2.floorKey(1))(nm1.floorKey(1))
    expect(nm2.floorKey(2))(nm1.floorKey(2))
    expect(nm2.floorKey(3))(nm1.floorKey(3))
    expect(nm2.floorKey(4))(nm1.floorKey(4))
    expect(nm2.floorKey(6))(nm1.floorKey(6))
  }

  it should "work for size and, iterators, and keys when the filter is in the middle" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 3
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
  }

  it should "work for contains, get, ceilingKey, higherKey, lowerKey, floorKey when the filter is in the middle" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 3
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
    expect(sm1.get(1))(nm1.get(1))
    expect(sm1.get(2))(nm1.get(2))
    expect(sm1.get(3))(nm1.get(3))
    expect(sm1.contains(1))(nm1.contains(1))
    expect(sm1.contains(2))(nm1.contains(2))
    expect(sm1.contains(3))(nm1.contains(3))

    val nm2 = NavigableMap(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    filterTests1(nm1, nm2)
  }

  it should "work for size and, iterators, and keys when the filter is in the top" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 5
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
  }

  it should "work for contains, get, ceilingKey, higherKey, lowerKey, floorKey when the filter is in the top" in {
    val baseMap = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val baseSortedMap = SortedMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "FIVE", 5 -> "Five")
    val filter1: (Int) => Boolean = _ != 5
    val nm1 = baseMap.withFilterKeys(filter1)
    val sm1 = baseSortedMap.filterKeys(filter1)
    expect(sm1)(nm1)
    expect(sm1.size)(nm1.size)
    expect(sm1.iterator.toList)(nm1.iterator.toList)
    expect(sm1.keys.toList)(nm1.keys.toList)
    expect(sm1.get(1))(nm1.get(1))
    expect(sm1.get(2))(nm1.get(2))
    expect(sm1.get(3))(nm1.get(3))
    expect(sm1.contains(1))(nm1.contains(1))
    expect(sm1.contains(2))(nm1.contains(2))
    expect(sm1.contains(3))(nm1.contains(3))

    val nm2 = NavigableMap(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four")
    filterTests1(nm1, nm2)
  }


  def testWithRandomValues(trials: Int, seed: Int, repetitions: Int) {
    val randomGenerator: Random = new Random(seed)
    val testValues = new ArrayBuffer[(Int, String)](trials)

    val navigableBuilder = NavigableMap.newBuilder[Int, String]

    for (i <- 0 until trials) {
      val random: Int = (randomGenerator nextInt ())
      testValues += ((random, "Random " + random))
    }

    navigableBuilder ++= testValues
    val navigableMap = navigableBuilder result ()

    for (j <- 0 until repetitions; i <- 0 until trials) {
      expect(testValues(i)._2)(navigableMap.get(testValues(i)._1).get)
    }
  }

  // TODO Make sure that ++ returns a NavigableMap and not Map

  def testNResults(n: Int) {
    val builder = NavigableMap.newBuilder[Key, Value]
    val subtest = testValues.take(n)
    builder ++= subtest
    val map = builder result ()
    assert(map.size == subtest.length)
    subtest.foreach[Unit] {
      entry =>
        expect(Some(entry._2)) {
          map.get(entry._1)
        }
    }
  }
}
