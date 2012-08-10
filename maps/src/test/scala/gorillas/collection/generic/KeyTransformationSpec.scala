package gorillas.collection.generic

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import gorillas.collection.generic.KeyTransformation.StringKeyTransformation
import util.Random

class KeyTransformationSpec extends FunSpec with ShouldMatchers {

  describe("StringKeyTransformation") {

    val transformer = StringKeyTransformation
    val ordering = scala.math.Ordering.String

    it("should transform the keys in the same order") {
      val rand1 = new Random
      val rand2 = new Random
      val rand3 = new Random
      val rand4 = new Random

      for (i <- 0 to 100000) {
        val string1 = rand1.nextString(rand3.nextInt(10))
        val string2 = rand2.nextString(rand4.nextInt(10))
        val val1 = transformer.transform(string1)
        val val2 = transformer.transform(string2)
        expect(ordering.lt(string1, string2))(val1 < val2)
      }

    }
  }

}
