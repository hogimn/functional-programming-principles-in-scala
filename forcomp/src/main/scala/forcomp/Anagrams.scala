package forcomp

import scala.io.{ Codec, Source }

/**
 * This object implements the AnagramsInterface and provides functionalities for working with anagrams.
 */
object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   *
   * @param w The word to convert.
   * @return The occurrence list of the word.
   */
  def wordOccurrences(w: Word): Occurrences =
    // Convert the word to lowercase
    w.toLowerCase
      // Group the characters by their identity.
      .groupBy(identity)
      // Map each character and its corresponding list of characters to a pair of character and length.
      .map((char, chars) => (char, chars.length))
      // Convert the resulting map to a list of pairs.
      .toList
      // Sort the list of pairs alphabetically based on the character.
      .sortBy((char, _) => char)

  /**
   * Converts a sentence into its character occurrence list.
   *
   * @param s The sentence to convert.
   * @return The character occurrence list of the sentence.
   */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.foldLeft("")(_ + _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   * @return The map from occurrences to words.
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences)

  /**
   * Returns all the anagrams of a given word.
   *
   * @param word The word to find anagrams for.
   * @return The list of anagrams of the given word.
   */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   *
   * @param occurrences The occurrence list to generate subsets from.
   * @return The list of all subsets of the occurrence list.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match
    case (char, freq) :: tail =>
      // For each frequency value from 0 to the maximum frequency
      for
        f <- (0 to freq).toList
        // Generate subsets from the remaining tail of the occurrence list
        rest <- combinations(tail)
      yield
        // If the frequency is less than 1, exclude the current character from the subset
        if f < 1 then
          rest
        // Otherwise, include the character with the current frequency in the subset
        else
          (char, f) :: rest
    case Nil =>
      // If the occurrence list is empty, return a single element list containing an empty list
      List(Nil)

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   * @param x The occurrence list to subtract from.
   * @param y The occurrence list to subtract.
   * @return The resulting occurrence list after subtracting `y` from `x`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    // For each character and frequency in `x`
    (for
      (char, freq) <- x
      // Filter out characters that appear in `y` with a frequency greater than their frequency in `y`
      if freq > y.toMap.getOrElse(char, 0)
    yield
      // Subtract the corresponding frequency in `y` from the frequency in `x`
      (char, freq - y.toMap.getOrElse(char, 0)))
      // Sort the resulting occurrence list by character
      .sorted

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   *
   * @param sentence The input sentence for which anagrams are generated.
   * @return A list of all possible anagram sentences.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    /**
     * Inner recursive function that generates anagram sentences for a given occurrence list.
     *
     * @param occ The occurrence list representing the remaining characters to be used.
     * @return A list of all possible anagram sentences.
     */
    def sentenceAnagramsOcc(occ: Occurrences): List[Sentence] = occ match
      // If the occurrence list is empty, return a list with an empty sentence (base case).
      case Nil => List(Nil)
      // If there are remaining characters in the occurrence list, generate anagrams.
      case _ =>
        // For each possible occurrence subset (non-empty) of the occurrence list
        for
          newOcc <- combinations(occ) if newOcc.nonEmpty
          // For each word that corresponds to the occurrence subset
          word <- dictionaryByOccurrences.getOrElse(newOcc, Nil)
          // Recursively generate anagrams for the remaining characters after subtracting the word's occurrence
          rest <- sentenceAnagramsOcc(subtract(occ, wordOccurrences(word)))
        yield
          // Construct the anagram sentence by appending the word to the recursive result
          word :: rest

    // Start the anagram generation process with the occurrence list of the input sentence
    sentenceAnagramsOcc(sentenceOccurrences(sentence))

/**
 * The `Dictionary` object provides functionality related to loading the dictionary file.
 */
object Dictionary:
  /**
   * Loads the dictionary from a file and returns it as a list of words.
   *
   * @return The dictionary as a list of words.
   */
  def loadDictionary: List[String] =
    // Try to read the dictionary file from the resources directory
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      // Throw an error if the dictionary file is not found
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      // Read the lines from the input stream using UTF-8 encoding
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      // Convert the lines to a list of words
      s.getLines().toList
    catch
      case e: Exception =>
        // Handle any exceptions that occur during file reading
        println("Could not load word list: " + e)
        throw e
    finally
      // Close the input stream in the finally block to ensure it's always closed
      wordstream.close()
