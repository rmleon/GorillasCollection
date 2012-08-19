package gorillas.collection.immutable

/**
 * Created with IntelliJ IDEA.
 * User: rleon
 * Date: 8/16/12
 * Time: 12:59 AM
 * To change this template use File | Settings | File Templates.
 */

trait NavigableMapLike[K, +V] {

  /**
   * @param key lookup key
   * @return The greatest key less than or equal to the given key
   */
  def floorKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The least key strictly greater than the given key
   */
  def higherKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The least key greater than or equal to the given key
   */
  def ceilingKey(key: K): Option[K]

  /**
   * @param key lookup key
   * @return The greatest key strictly less than the given key
   */
  def lowerKey(key: K): Option[K]

}
