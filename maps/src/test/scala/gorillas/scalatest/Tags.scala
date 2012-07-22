package gorillas.scalatest

import org.scalatest.Tag

/**
 * ScalaTest tags to classify the different types of test
 */
object Tags {

  /**
   * These test depend on the JVM settings
   */
  object Memory extends Tag("Memory")

  /**
   * These test might fail in some machines and pass in others.
   */
  object Performance extends Tag("NonDeterministic")
}
