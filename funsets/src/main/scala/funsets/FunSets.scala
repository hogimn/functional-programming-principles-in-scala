package funsets

import scala.annotation.tailrec

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:

  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   *
   * @param s    The set to check.
   * @param elem The element to check for containment.
   * @return True if the set contains the element, false otherwise.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   *
   * @param elem The element to create a set from.
   * @return A set that contains only the given element.
   */
  def singletonSet(elem: Int): FunSet =
    x => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   *
   * @param s The first set.
   * @param t The second set.
   * @return The union of the two sets.
   */
  def union(s: FunSet, t: FunSet): FunSet =
    x => contains(s, x) || contains(t, x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   *
   * @param s The first set.
   * @param t The second set.
   * @return The intersection of the two sets.
   */
  def intersect(s: FunSet, t: FunSet): FunSet =
    x => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   *
   * @param s The first set.
   * @param t The second set.
   * @return The difference of the two sets.
   */
  def diff(s: FunSet, t: FunSet): FunSet =
    x => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   *
   * @param s The set to filter.
   * @param p The predicate to apply.
   * @return The subset of `s` that satisfies the predicate `p`.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet =
    x => contains(s, x) && p(x)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   *
   * @param s The set to check.
   * @param p The predicate to apply to each element.
   * @return True if all bounded integers within `s` satisfy `p`, false otherwise.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    @tailrec
    def iter(a: Int): Boolean =
      // Base case: If `a` is outside the range of -bound to bound, return true.
      if a < -bound || bound < a then
        true
      // Recursive case 1: If `a` is in the set `s`, check if `p(a)` is true,
      // and recursively call `iter` with `a + 1`.
      else if contains(s, a) then
        p(a) && iter(a + 1)
      // Recursive case 2: If `a` is not in the set `s`, recursively call `iter` with `a + 1`.
      else
        iter(a + 1)

    iter(-bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   *
   * @param s The set to check.
   * @param p The predicate to apply to each element.
   * @return True if there exists a bounded integer within `s` that satisfies `p`, false otherwise.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   *
   * @param s The set to transform.
   * @param f The function to apply to each element.
   * @return The transformed set.
   */
  def map(s: FunSet, f: Int => Int): FunSet =
    x => exists(s, a => f(a) == x)

  /**
   * Displays the contents of a set
   *
   * @param s The set to display.
   * @return A string representation of the set.
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   *
   * @param s The set to display.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

/**
 * The `FunSets` object serves as the entry point for the functional set operations.
 * It extends the `FunSets` trait which provides the interface for the set functions.
 */
object FunSets extends FunSets
