package gorillas.collection.immutable

import util.Random
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import gorillas.collection.generic.KeyTransformation
import gorillas.scalatest.Tags

/**
 * Non-deterministic tests used to make sure that the Map and the builder doesn't blow up the memory when storing big quantities of data.
 * @author Ricardo Leon
 */

class NavigableMapMemorySpecs extends FlatSpec with ShouldMatchers {
  type Key = Int

  type Value = String

  implicit val trans = new KeyTransformation[Key] {
    def transform(x: Key) = x
  }

  /**
   * The following test requires -Xmx512mb
   */
  "NavigableMapBuilder" should "be able to hold 10,000,000 (random) objects without running out of memory" taggedAs (Tags.Memory) in {
    val randomGenerator: Random = new Random(347)
    val navigableBuilder = NavigableMap.newBuilder[Int, String]
    var i = 0
    while (i < 10000000) {
      val random: Int = (randomGenerator nextInt ())
      navigableBuilder += ((random, "Random "))
      i += 1
    }
    val navigableMap = navigableBuilder result ()
  }

  /**
   * The following code throws OutOfMemoryError (using -Xmx512mb)
   */
  //    "Map" should "be able to hold 10,000,000 (random) objects without running out of memory" in {
  //      val randomGenerator: Random = new Random(347)
  //      val builder = scala.collection.mutable.Map.newBuilder[Int, String]
  //      var i = 0
  //      while (i < 10000000) {
  //        val random: Int = (randomGenerator nextInt())
  //        builder += ((random, "Random "))
  //        i += 1
  //      }
  //      val map = builder result()
  //    }
}
