package computationalcognitivescience.coherencecommunication.util

object SetTheoryDev {
  /** Returns all elements from the set that have minimum value according to a given function `f `.
   *
   * The function `f ` needs to have as an argument the same type as the elements in the set (A),
   * and returns a value for which an ordering exists (e.g., a number or String).
   * @param set
   *   The set to select maximum elements from.
   * @param f
   *   The function for which the elements are evaluated.
   * @param ord
   *   An ordering on the return values of `f `, this can be omitted when using simple types such
   *   as numbers.
   * @tparam A
   *   The type of the elements in the set.
   * @tparam T
   *   The type of the values that are returned by `f `.
   * @return
   *   A set of elements that have maximum value according to `f `. Can contain zero, one or more
   *   elements.
   */
  def argMin[A, T](set: Set[A], f: A => T)(implicit ord: Ordering[T]): Set[A] = {
    if (set.isEmpty) set
    else {
      val min = set.map(f).min // find max value
      set.filter(f(_) == min) // return all elems with max value
    }
  }
}
