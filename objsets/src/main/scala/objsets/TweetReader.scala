package objsets

/**
 * A utility object that provides methods for reading and processing tweets.
 * It acts as a reader and parser for tweet data.
 * This object allows retrieving and manipulating tweet data from various sources.
 */
object TweetReader:

  /**
   * A utility object that provides methods for parsing and reading tweets.
   */
  object ParseTweets:
    /**
     * Parses the input string using regular expressions and returns a list of tweet data as maps.
     *
     * @param s The input string containing tweet data in a specific format.
     * @return A list of tweet data as maps, where each map represents a tweet with user, text, and retweets.
     */
    def regexParser(s: String): List[Map[String, Any]] =
      // In real life. you would use an actual JSON library...
      // Declare a regular expression pattern to match the tweet data
      val tweetRegex = """^\{ .*"user": "([^"]+)", "text": "([^"]+)", "retweets": ([\\.0-9]+) \},?$""".r
      // Split the input string into lines, convert it to a list, and remove the first and last lines
      s.split("\r?\n").toList.tail.init.map {
        // Pattern matching to extract tweet data using the tweetRegex pattern
        // Create a map representing a tweet with keys "user", "text", and "retweets"
        case tweetRegex(user, text, retweets) => Map("user" -> user, "text" -> text, "retweets" -> retweets.toDouble)
      }

    /**
     * Retrieves a list of tweets for a given user from the provided JSON string.
     *
     * @param user The username of the tweets' owner.
     * @param json The JSON string containing tweet data.
     * @return A list of Tweet objects containing user, text, and retweets count.
     */
    def getTweets(user: String, json: String): List[Tweet] =
      // Iterate over each map in the list returned by regexParser(json)
      for map <- regexParser(json) yield
        // Extract 'text' and 'retweet_count' from the map
        // I think the key name 'retweet_count' should be replaced with 'retweets' based on regexParser function above
        val text = map("text")
        val retweets = map("retweet_count")
        // Create a new Tweet object using the extracted values
        Tweet(user, text.toString, retweets.toString.toDouble.toInt)

    /**
     * Retrieves a list of tweets' data for a given user from the provided JSON string.
     *
     * @param user The username of the tweets' owner.
     * @param json The JSON string containing tweet data.
     * @return A list of Tweet objects containing user, text, and retweets count.
     */
    def getTweetData(user: String, json: String): List[Tweet] =
      // is list
      // Create a list 'l' by calling regexParser(json)
      val l = regexParser(json)
      // Iterate over each map 'map' in the list 'l' and yield a value for each iteration
      for map <- l yield
        // Extract 'text' and 'retweets' from the map
        val text = map("text")
        val retweets = map("retweets")
        // Create a new Tweet object using the extracted values
        Tweet(user, text.toString, retweets.toString.toDouble.toInt)

  /**
   * Converts a list of tweets to a TweetSet.
   *
   * @param l The list of tweets to be converted.
   * @return A TweetSet containing all the tweets from the input list.
   */
  def toTweetSet(l: List[Tweet]): TweetSet =
  // Start with an Empty TweetSet and fold over the list of tweets
    l.foldLeft(Empty(): TweetSet)(_.incl(_))

  /**
   * Converts a list of tweets to a JSON string representation.
   *
   * @param tws The list of tweets to be converted.
   * @return A JSON string representing the tweets.
   */
  def unparseToData(tws: List[Tweet]): String =
    // Create a mutable StringBuffer to store the resulting string
    val buf = StringBuffer()
    // Iterate over each tweet in the input list 'tws'
    for tw <- tws do
      // Generate a JSON string representing the tweet
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
                                    tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
                                    tw.retweets + ".0 }"
      // Append the generated JSON string to the buffer, followed by a comma and a newline
      buf.append(json + ",\n")
    // Convert the StringBuffer to a String and return
    buf.toString

  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  /**
   * Store the tweet data for the "gizmodo" site using the getTweetData method with the user and tweet data arguments
   */
  private val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
  /**
   * Store the tweet data for the "TechCrunch" site using the getTweetData method with the user and tweet data arguments
   */
  private val techCrunchTweets = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
  /**
   * Store the tweet data for the "engadget" site using the getTweetData method with the user and tweet data arguments
   */
  private val engadgetTweets = TweetReader.ParseTweets.getTweetData("engadget", TweetData.engadget)
  /**
   * Store the tweet data for the "amazondeals" site using the getTweetData method with the user and tweet data arguments
   */
  private val amazondealsTweets = TweetReader.ParseTweets.getTweetData("amazondeals", TweetData.amazondeals)
  /**
   * Store the tweet data for the "CNET" site using the getTweetData method with the user and tweet data arguments
   */
  private val cnetTweets = TweetReader.ParseTweets.getTweetData("CNET", TweetData.CNET)
  /**
   * Store the tweet data for the "gadgetlab" site using the getTweetData method with the user and tweet data arguments
   */
  private val gadgetlabTweets = TweetReader.ParseTweets.getTweetData("gadgetlab", TweetData.gadgetlab)
  /**
   * Store the tweet data for the "mashable" site using the getTweetData method with the user and tweet data arguments
   */
  private val mashableTweets = TweetReader.ParseTweets.getTweetData("mashable", TweetData.mashable)

  /**
   * Create a list containing all the tweet data from different sources
   */
  private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  /**
   * Create a map that associates site names with their respective tweet lists
   */
  val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq((sites(0) -> gizmodoTweets),
                 (sites(1) -> techCrunchTweets),
                 (sites(2) -> engadgetTweets),
                 (sites(3) -> amazondealsTweets),
                 (sites(4) -> cnetTweets),
                 (sites(5) -> gadgetlabTweets),
                 (sites(6) -> mashableTweets))

  /**
   * Convert each tweet list in the sources list to a TweetSet using the toTweetSet function,
   * and store the results in the tweetSets list
   */
  val tweetSets: List[TweetSet] = sources.map(tweets => toTweetSet(tweets))

  /**
   * Create a map called siteTweetSetMap by combining the sites list and tweetSets list using the zip operation,
   * where each site name is associated with its corresponding TweetSet.
   */
  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  /**
   * Computes the union of all TweetSets in the curSets list.
   *
   * @param curSets The list of TweetSets to compute the union from.
   * @param acc     The accumulator TweetSet to accumulate the union.
   * @return The union of all TweetSets in the curSets list.
   */
  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet =
    // If the curSets list is empty, it returns the accumulator.
    if curSets.isEmpty then
      acc
    // Otherwise, it recursively calls unionOfAllTweetSets with the tail of curSets
    // and the union of the head of curSets with the accumulator.
    else
      unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))

  /**
   * Compute the union of all TweetSets in the tweetSets list by calling the unionOfAllTweetSets function,
   * starting with an empty TweetSet (Empty()) as the initial accumulator.
   * The result is stored in the variable allTweets, representing the union of all the TweetSets.
   */
  val allTweets: TweetSet = unionOfAllTweetSets(tweetSets, Empty())
