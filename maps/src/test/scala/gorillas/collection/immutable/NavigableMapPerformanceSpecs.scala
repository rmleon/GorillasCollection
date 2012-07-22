package gorillas.collection.immutable

import gorillas.collection.generic.KeyTransformation
import util.Random
import scala.collection.mutable.ArrayBuffer
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable

/**
 * Non-deterministic test to make sure that the map performs at least as well as the Scala maps
 * @author Ricardo Leon
 */
class NavigableMapPerformanceSpecs extends FlatSpec with ShouldMatchers {
  type Key = Int

  type Value = String

  implicit val trans = new KeyTransformation[Key] {
    def transform(x: Key) = x
  }

  "A single-entry NavigableMap" should "perform as fast (with a 10% error margin) as immutable.Map when retrieving the element 10 million times" in perfTest(1, 13, 10000000, 10)

  it should "perform as fast as the normal empty map (with a 10% error margin) when doing 100 million iterations" in {
    val repetitions = 100000000
    // System.gc() // Start with a clean slate
    val deltaNormal = runTestOne(repetitions)
    //System.gc() // Start with a clean slate
    val deltaNavigable = runTestTwo(repetitions)
    println("Normal map took %d ms and navigable map took %d ms".format(deltaNormal, deltaNavigable))
    assert(deltaNormal * 1.1 >= deltaNavigable, "Single-entry Navigation map was not faster (normal was %d vs. Navigable %d)".format(deltaNormal, deltaNavigable))
  }

  "A NavigableMap" should "outperform immutable.Maps when reading 500k objects 10 times each " in perfTest(500000, 41, 10)

  "A NavigableMapBuilder" should "outperform Map.Builder when creating 500k objects" in testBuilders(500000)

  def perfTest(trials: Int, seed: Int, repetitions: Int, marginErrorPercentage: Int = 0) {
    val randomGenerator: Random = new Random(seed)
    val testValues = new ArrayBuffer[(Int, String)](trials)

    for (i <- 0 until trials) {
      val random: Int = (randomGenerator nextInt())
      testValues.append((random, "Hello " + random))
    }

    val navigableBuilder = NavigableMap.newBuilder[Int, String]
    navigableBuilder ++= testValues
    val navigableMap = navigableBuilder result()

    val mapBuilder = Map.newBuilder[Int, String]
    mapBuilder ++= testValues
    val map = mapBuilder result()

    //assert(navigableMap.length == testValues.length, "Test results and navigableMap don't have the same size")
    //System.gc() // Start with a clean slate
    val startTimeNormalMap = System.currentTimeMillis()
    for (j <- 0 until repetitions; i <- 0 until trials) {
      assert(map.get(testValues(i)._1).get == testValues(i)._2)
    }
    val endTimeNormalMap = System.currentTimeMillis()

    //System.gc() // Start with a clean slate
    val startTimeNavigableMap = System.currentTimeMillis()
    for (j <- 0 until repetitions; i <- 0 until trials) {
      assert(navigableMap.get(testValues(i)._1).get == testValues(i)._2)
    }
    val endTimeNavigableMap = System.currentTimeMillis()

    val deltaNormalMap = (endTimeNormalMap - startTimeNormalMap)
    val deltaNavigableMap = (endTimeNavigableMap - startTimeNavigableMap)

    println("Normal map took %d ms (for %d entries) and navigable map took %d ms (for %d entries)".format(deltaNormalMap, map.size, deltaNavigableMap, navigableMap.size))

    assert(deltaNormalMap * (1.0 + marginErrorPercentage / 100.0) > deltaNavigableMap, "NavigationMap was not faster (Map time: %d vs. NavigableMap time %d)".format(deltaNormalMap, deltaNavigableMap))
  }

  def runTestOne(repetitions: Int) = {
    val mapBuilder = Map.newBuilder[Int, String]
    val map = mapBuilder result()
    val startTimeNormalMap = System.currentTimeMillis()
    (0 until repetitions).foreach[Unit](
      j => assert(map.get(j) == None)
    )
    val endTimeNormalMap = System.currentTimeMillis()
    endTimeNormalMap - startTimeNormalMap
  }

  def runTestTwo(repetitions: Int) = {
    val navigableBuilder = NavigableMap.newBuilder[Int, String]
    val navigableMap = navigableBuilder result()
    val startTimeNavigableMap = System.currentTimeMillis()
    (0 until repetitions).foreach[Unit](
      j => assert(navigableMap.get(j) == None)
    )
    val endTimeNavigableMap = System.currentTimeMillis()
    endTimeNavigableMap - startTimeNavigableMap
  }

  def createMap[M](tests: ArrayBuffer[(Int, String)], builder: mutable.Builder[(Int, String), M]): Long = {
    //System.gc() // Start with a clean slate
    val startTimeNavigableMap = System.currentTimeMillis()
    builder ++= tests
    val map = builder result()
    val endTimeNavigableMap = System.currentTimeMillis()
    builder.clear()
    endTimeNavigableMap - startTimeNavigableMap
  }

  def testBuilders(n: Int) {
    val random = new Random(347 * n)
    val testValues = new ArrayBuffer[(Int, String)]
    testValues.sizeHint(n)
    (0 until n).foreach {
      i =>
        testValues += (random.nextInt() -> "Random")
    }
    val navigableBuilder = NavigableMap.newBuilder[Int, String]
    val normalMapBuilder = Map.newBuilder[Int, String]
    val normalResults = createMap[Map[Int, String]](testValues, normalMapBuilder)
    val navigableResults = createMap[NavigableMap[Int, String]](testValues, navigableBuilder)
    assert(navigableResults < normalResults, "Navigable builder should be faster than normal builder (Navigable %d vs Normal %d)".format(navigableResults, normalResults))
    println("Navigable took %d and Map took %d".format(navigableResults, normalResults))
  }

}
