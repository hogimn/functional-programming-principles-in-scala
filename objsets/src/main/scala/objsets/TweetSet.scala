package objsets

import TweetReader.*

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface:

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   *
   * @param p The predicate function to be applied to each element in the set.
   * @return A subset of elements that satisfy the given predicate.
   */
  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, Empty())

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   *
   * @param p   The predicate function to apply to each tweet.
   * @param acc The accumulated tweet set.
   * @return A new `TweetSet` containing tweets that satisfy the predicate.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   *
   * @param that The `TweetSet` to be combined with `this` set.
   * @return A new `TweetSet` containing all the tweets from both `this` set and `that` set.
   */
  def union(that: TweetSet): TweetSet =
    filterAcc(_ => true, that)

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   *
   * @return A `TweetList` containing all the tweets in descending order of retweet count.
   */
  def descendingByRetweet: TweetList =
    descendingByRetweetAcc(Nil)

  /**
   * A helper method for `descendingByRetweet` that accumulates the tweets in descending order.
   *
   * @param acc The accumulated `TweetList` in descending order of retweet count.
   * @return A `TweetList` containing all the tweets in descending order of retweet count.
   */
  def descendingByRetweetAcc(acc: TweetList): TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   *
   * @param tweet The `Tweet` to be included in the set.
   * @return A new `TweetSet` with the added `tweet` if it didn't already exist, or the current set if it does.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   *
   * @param tweet The `Tweet` to be removed from the set.
   * @return A new `TweetSet` without the specified `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   *
   * @param tweet The `Tweet` to be checked for existence in the set.
   * @return `true` if the `tweet` exists in the set, `false` otherwise.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   *
   * @param f The function to be applied to each element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

/**
 * Represents an empty set of tweets.
 */
class Empty extends TweetSet:

  /**
   * Filters the elements in the set by applying the given predicate and returns the accumulated set.
   *
   * @param p   The predicate function to apply on each tweet.
   * @param acc The accumulated set of tweets satisfying the predicate.
   * @return The accumulated set of tweets satisfying the predicate.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  /**
   * Checks if the set contains a specific tweet.
   *
   * @param tweet The tweet to check for existence in the set.
   * @return `false` since the set is empty and does not contain any tweets.
   */
  def contains(tweet: Tweet): Boolean = false

  /**
   * Adds a tweet to the set, creating a new non-empty set.
   *
   * @param tweet The tweet to be included in the set.
   * @return A new non-empty `TweetSet` containing only the specified tweet.
   */
  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  /**
   * Removes a tweet from the set.
   *
   * @param tweet The tweet to be removed from the set.
   * @return The same empty set since there are no tweets to remove.
   */
  def remove(tweet: Tweet): TweetSet = this

  /**
   * Applies a function to each element in the set.
   *
   * @param f The function to be applied to each element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = ()

  /**
   * Throws a `NoSuchElementException` since the set is empty and does not have the most retweeted tweet.
   *
   * @throws NoSuchElementException when called on an empty set.
   */
  override def mostRetweeted: Tweet =
    throw new NoSuchElementException()

  /**
   * Returns the tweets in descending order based on retweet count.
   *
   * @param acc The accumulated `TweetList` that stores the tweets in descending order based on retweet count.
   * @return The accumulated `TweetList` with the tweets in descending order based on retweet count.
   */
  override def descendingByRetweetAcc(acc: TweetList): TweetList = acc

/**
 * Represents a non-empty set of tweets.
 *
 * @param elem  The tweet at the root of this set.
 * @param left  The left subtree of tweets.
 * @param right The right subtree of tweets.
 */
class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:

  /**
   * Filters the elements in the set by applying the given predicate and returns the accumulated set.
   *
   * @param p   The predicate function to apply on each tweet.
   * @param acc The accumulated set of tweets satisfying the predicate.
   * @return The accumulated set of tweets satisfying the predicate.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    // If the predicate is true for the current element (elem),
    // recursively filter the right and left subtrees and include elem in the accumulated set (acc)
    if p(elem) then
      right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    // If the predicate is false for the current element (elem),
    // recursively filter the right and left subtrees without including elem in the accumulated set (acc)
    else
      right.filterAcc(p, left.filterAcc(p, acc))

  /**
   * The following methods are already implemented
   */

  /**
   * Checks if the set contains a specific tweet.
   *
   * @param x The tweet to check for existence in the set.
   * @return `true` if the set contains the tweet, `false` otherwise.
   */
  def contains(x: Tweet): Boolean =
    // If the given tweet (x) is lexicographically less than the current element (elem),
    // check if it exists in the left subtree
    if x.text < elem.text then
      left.contains(x)
    // If the given tweet (x) is lexicographically greater than the current element (elem),
    // check if it exists in the right subtree
    else if elem.text < x.text then
      right.contains(x)
    // If the given tweet (x) matches the current element (elem), it exists in the set
    else true

  /**
   * Adds a tweet to the set.
   *
   * @param x The tweet to be included in the set.
   * @return A new `TweetSet` with the added tweet.
   */
  def incl(x: Tweet): TweetSet =
    // If the given tweet (x) is lexicographically less than the current element (elem),
    // add it to the left subtree recursively
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    // If the given tweet (x) is lexicographically greater than the current element (elem),
    // add it to the right subtree recursively
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    // If the given tweet (x) matches the current element (elem),
    // the tweet is already in the set, so return the current set
    else
      this

  /**
   * Removes a tweet from the set.
   *
   * @param tw The tweet to be removed from the set.
   * @return A new `TweetSet` with the specified tweet removed.
   */
  def remove(tw: Tweet): TweetSet =
    // If the text of the tweet to be removed (tw) is lexicographically less than the current element's tweet (elem),
    // remove the tweet from the left subtree recursively
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    // If the text of the tweet to be removed (tw) is lexicographically greater than the current element's tweet (elem),
    // remove the tweet from the right subtree recursively
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    // If the text of the tweet to be removed (tw) matches the current element's tweet (elem),
    // remove the current element by taking the union of the left and right subtrees
    else
      left.union(right)

  /**
   * Applies a function to each element in the set.
   *
   * @param f The function to be applied to each element in the set.
   */
  def foreach(f: Tweet => Unit): Unit =
    // Apply the function f to the current element's tweet (elem)
    f(elem)
    // Recursively apply the function to the left subtree
    left.foreach(f)
    // Recursively apply the function to the right subtree
    right.foreach(f)

  /**
   * Returns the tweet with the highest retweet count in the set.
   *
   * @return The tweet with the highest retweet count.
   * @throws NoSuchElementException when called on an empty set.
   */
  override def mostRetweeted: Tweet =
    // Initialize the maximum tweet to the current element's tweet (elem)
    var max = this.elem
    // Apply a function to each element in the set
    foreach(tweet =>
      // Check if the current tweet (tweet) has a higher number of retweets than the maximum tweet (max)
      if tweet.retweets >= max.retweets then
      // Update the maximum tweet to the current tweet (tweet) if it has a higher retweet count
        max = tweet
    )
    // Return the tweet with the highest retweet count
    max

  /**
   * Returns the tweets in descending order based on retweet count.
   *
   * @param acc The accumulated `TweetList` that stores the tweets in descending order based on retweet count.
   * @return The accumulated `TweetList` with the tweets in descending order based on retweet count.
   */
  override def descendingByRetweetAcc(acc: TweetList): TweetList =
    // Find the tweet with the highest retweet count
    val max = mostRetweeted
    // Add the tweet with the highest retweet count to the beginning of the accumulated tweet list (acc)
    // and recursively call descendingByRetweetAcc on the updated tweet set (after removing the max tweet)
    Cons(max, remove(max).descendingByRetweetAcc(acc))


/**
 * A trait representing a list of tweets.
 * The list can either be an instance of `Cons` or `Nil`.
 */
trait TweetList:
  /**
   * Returns the head of the tweet list.
   *
   * @return The head of the tweet list.
   */
  def head: Tweet

  /**
   * Returns the tail of the tweet list.
   *
   * @return The tail of the tweet list.
   */
  def tail: TweetList

  /**
   * Checks if the tweet list is empty.
   *
   * @return `true` if the tweet list is empty, `false` otherwise.
   */
  def isEmpty: Boolean

  /**
   * Applies a function to each element in the tweet list.
   *
   * @param f The function to be applied to each element in the tweet list.
   */
  def foreach(f: Tweet => Unit): Unit =
  // If the tweet list is not empty
    if !isEmpty then
      // Apply the function to the head of the tweet list
      f(head)
      // Recursively call foreach on the tail of the tweet list
      tail.foreach(f)

/**
 * An object representing an empty tweet list.
 */
object Nil extends TweetList:
  /**
   * Throws a `NoSuchElementException` since there is no head in an empty list.
   *
   * @throws NoSuchElementException Always throws this exception.
   */
  def head = throw java.util.NoSuchElementException("head of EmptyList")

  /**
   * Throws a `NoSuchElementException` since there is no tail in an empty list.
   *
   * @throws NoSuchElementException Always throws this exception.
   */
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")

  /**
   * Returns `true` since the empty list is indeed empty.
   *
   * @return `true` indicating that the list is empty.
   */
  def isEmpty = true

/**
 * A class representing a non-empty tweet list.
 *
 * @param head The head of the tweet list.
 * @param tail The tail of the tweet list.
 */
class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  /**
   * Returns `false` since a non-empty list is not empty.
   *
   * @return `false` indicating that the list is not empty.
   */
  def isEmpty = false


/**
 * An object representing a comparison between Google and Apple.
 */
object GoogleVsApple:
  /**
   * A list of keywords related to Google.
   */
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  /**
   * A list of keywords related to Apple.
   */
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  /**
   * Lazily evaluates and returns a `TweetSet` containing all tweets mentioning
   * any of the keywords related to Google.
   *
   * @return A `TweetSet` containing tweets related to Google.
   */
  lazy val googleTweets: TweetSet = allTweets
    .filter(t => google.exists(keyword => t.text.contains(keyword)))

  /**
   * Lazily evaluates and returns a `TweetSet` containing all tweets mentioning
   * any of the keywords related to Apple.
   *
   * @return A `TweetSet` containing tweets related to Apple.
   */
  lazy val appleTweets: TweetSet = allTweets
    .filter(t => apple.exists(keyword => t.text.contains(keyword)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   *
   * @return A `TweetList` containing tweets mentioning Apple or Google, sorted by retweet count.
   */
  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet

/**
 * The main entry point of the program.
 */
object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
