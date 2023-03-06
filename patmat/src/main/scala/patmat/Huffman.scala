package patmat

import scala.annotation.tailrec

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
/**
 * Represents a `Fork` node in the huffman tree.
 *
 * @param left   The left subtree of the fork.
 * @param right  The right subtree of the fork.
 * @param chars  The list of characters present in the leaves below this fork.
 * @param weight The weight of the fork, which is the sum of the weights of the leaves below it.
 */
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

/**
 * Represents a `Leaf` node in the huffman tree.
 *
 * @param char   The character represented by this leaf.
 * @param weight The weight of the leaf, which is the frequency of appearance of the character.
 */
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface:

  // Part 1: Basics

  /**
   * Computes the weight of a given `CodeTree`.
   *
   * @param tree The `CodeTree` for which to compute the weight.
   * @return The weight of the `CodeTree`.
   */
  def weight(tree: CodeTree): Int = tree match
    case Leaf(_, weight) =>
      weight
    case Fork(_, _, _, weight) =>
      weight

  /**
   * Retrieves the characters contained in a given `CodeTree`.
   *
   * @param tree The `CodeTree` from which to retrieve the characters.
   * @return A list of characters contained in the `CodeTree`.
   */
  def chars(tree: CodeTree): List[Char] = tree match
    case Leaf(char, _) =>
      List(char)
    case Fork(_, _, chars, _) =>
      chars

  /**
   * Creates a new `CodeTree` by combining two existing `CodeTree`s.
   *
   * @param left  The left subtree of the new `CodeTree`.
   * @param right The right subtree of the new `CodeTree`.
   * @return The newly created `CodeTree` with the combined subtrees.
   */
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   *
   * @param str The input string.
   * @return A list of characters obtained from the input string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   *
   * @param chars The list of characters.
   * @return A list of pairs, where each pair consists of a character and its count of occurrences.
   */
  def times(chars: List[Char]): List[(Char, Int)] =
    /**
     * Helper function to compute the count of occurrences for each character.
     *
     * @param chars The remaining list of characters to process.
     * @param acc   The accumulated list of pairs (character, count).
     * @return A list of pairs, where each pair consists of a character and its count of occurrences.
     */
    @tailrec
    def timesAcc(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = chars match
      // Base case: Empty character list, return the accumulated result.
      case Nil =>
        acc
      case x :: xs =>
        timesAcc(xs, incrementCharCount(x, acc))

    /**
     * Helper function to increment the count of occurrences for a specific character.
     *
     * @param char The character for which to increment the count.
     * @param acc  The accumulated list of pairs (character, count).
     * @return A list of pairs, where each pair consists of a character and its count of occurrences.
     */
    def incrementCharCount(char: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match
      case (key, count) :: tail =>
        // If the character is already in the list, increment its count.
        if key == char then
          (key, count + 1) :: tail
        // Otherwise, keep the pair as it is and continue recursively.
        else
          (key, count) :: incrementCharCount(char, tail)
      // Base case: Character not found in the accumulated list, add it with count 1.
      case Nil => (char, 1) :: Nil

    timesAcc(chars, Nil)

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   *
   * @param freqs The frequency table, a list of pairs where each pair consists of a character and its frequency.
   * @return A list of `Leaf` nodes, ordered by ascending weights.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    /**
     * Helper function to create an ordered list of `Leaf` nodes.
     *
     * @param freqs The remaining frequency table to process.
     * @param acc   The accumulated list of `Leaf` nodes.
     * @return A list of `Leaf` nodes, ordered by ascending weights.
     */
    @tailrec
    def makeOrderedLeafListAcc(freqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = freqs match
      // Base case: Empty frequency table, return the accumulated result.
      case Nil =>
        acc
      case (char, weight) :: tail =>
        makeOrderedLeafListAcc(tail, insertLeaf(char, weight, acc))

    /**
     * Helper function to insert a `Leaf` node into the ordered list.
     *
     * @param char The character of the `Leaf` node to insert.
     * @param weight The weight of the `Leaf` node to insert.
     * @param acc  The accumulated list of `Leaf` nodes.
     * @return A list of `Leaf` nodes, ordered by ascending weights.
     */
    def insertLeaf(char: Char, weight: Int, acc: List[Leaf]): List[Leaf] = acc match
      // Base case: Empty list, add the `Leaf` node as the first element.
      case Nil =>
        Leaf(char, weight) :: Nil
      case Leaf(c, w) :: tail =>
        // If the weight is smaller or equal, insert the `Leaf` node before the current element.
        if weight <= w then
          Leaf(char, weight) :: Leaf(c, w) :: tail
        // Otherwise, keep the current element and continue recursively.
        else
          Leaf(c, w) :: insertLeaf(char, weight, tail)

    makeOrderedLeafListAcc(freqs, Nil)

  /**
   * Checks whether the list `trees` contains only one single code tree.
   *
   * @param trees The list of code trees to check.
   * @return `true` if the list contains only one tree, `false` otherwise.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match
    // Base case: List contains exactly one element.
    case head :: Nil =>
      true
    // List contains zero or more than one element.
    case _ =>
      false

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   *
   * @param trees The list of code trees to combine.
   * @return The resulting list of code trees after combining the first two elements.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match
    // Combine the first two elements into a `Fork` node and add it back to the remaining elements.
    case first :: second :: tail =>
      makeCodeTree(first, second) :: tail
    // Base case: List contains less than two elements, return unchanged.
    case _ =>
      trees

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * @param done  The termination condition function that determines when to stop.
   * @param merge The function that combines code trees.
   * @param trees The initial list of code trees to process.
   * @return The final list of code trees after applying the combination until the termination condition is met.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    // Base case: Termination condition is met, return the current list of trees.
    if done(trees) then
      trees
    // Recursively apply the merge function to the current list of trees.
    else
      until(done, merge)(merge(trees))

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   *
   * @param chars The text for which the optimal code tree is to be created.
   * @return The optimal code tree for encoding the text.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   *
   * @param tree The code tree used for decoding.
   * @param bits The bit sequence to decode.
   * @return The list of characters decoded from the bit sequence.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
    /**
     * Inner recursive function that performs the decoding process.
     *
     * @param node The current code tree node.
     * @param bits The remaining bit sequence to decode.
     * @param acc  The accumulated list of decoded characters.
     * @return The list of characters decoded from the bit sequence.
     */
    @tailrec
    def decodeAcc(node: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = node match
      // If the current node is a leaf, append the decoded character to the accumulator.
      case Leaf(char, _) =>
        decodeAcc(tree, bits, acc ::: List(char))
      // If the current node is a fork, traverse left or right based on the next bit in the sequence.
      case Fork(left, right, _, _) => bits match
        // If there are no more bits to decode, return the accumulated list of characters.
        case Nil => acc
        // If the next bit is 0, traverse left; otherwise, traverse right.
        case bit :: tail =>
          if (bit == 0)
            decodeAcc(left, tail, acc)
          else
            decodeAcc(right, tail, acc)

    // Start the decoding process with the given code tree and empty accumulator.
    decodeAcc(tree, bits, Nil)

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   *
   * @return The decoded secret message.
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   *
   * @param tree The code tree used for encoding.
   * @param text The text to be encoded.
   * @return The encoded sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    /**
     * Inner recursive function that performs the encoding process.
     *
     * @param node The current code tree node.
     * @param text The remaining text to encode.
     * @param acc  The accumulated list of encoded bits.
     * @return The encoded sequence of bits.
     */
    @tailrec
    def encodeAcc(node: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = text match
      // If there is no more text to encode, return the accumulated sequence of bits.
      case Nil =>
        acc
      // If there is more text to encode, check the current node and proceed accordingly.
      case char :: tail => node match
        // If the current node is a leaf, ignore the character and continue encoding with the tree.
        case Leaf(_, _) =>
          encodeAcc(tree, tail, acc)
        // If the current node is a fork, check if the character exists in the left or right subtree.
        case Fork(left, right, _, _) =>
          if containsChar(char)(left) then
            encodeAcc(left, text, acc ::: List(0))
          else if containsChar(char)(right) then
            encodeAcc(right, text, acc ::: List(1))
          else
            throw Error(s"$char unreachable")

    /**
     * Helper function that checks if a character exists in a given code tree.
     *
     * @param char The character to check.
     * @param tree The code tree to search in.
     * @return `true` if the character exists in the code tree, `false` otherwise.
     */
    def containsChar(char: Char)(tree: CodeTree): Boolean =
      /**
       * Inner recursive function that checks if a character exists in a list of characters.
       *
       * @param chars The list of characters to search in.
       * @return `true` if the character exists in the list, `false` otherwise.
       */
      @tailrec
      def charExists(chars: List[Char]): Boolean = chars match
        // If the character is found, return true.
        case x :: xs =>
          if x == char then
            true
          else
            charExists(xs)
        // If the end of the list is reached without finding the character, return false.
        case Nil =>
          false

      tree match
        // If the tree node is a leaf, compare the character with the leaf's stored character.
        case Leaf(c, _) =>
          char == c
        // If the tree node is a fork, check if the character exists in the list of characters.
        case Fork(_, _, chars, _) =>
          charExists(chars)

    // Start the encoding process with the given code tree and empty accumulator.
    encodeAcc(tree, text, Nil)

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   *
   * @param table The code table.
   * @param char  The character to find the bit sequence for.
   * @return The bit sequence representing the character.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match
    // If the table is empty, throw an error indicating that the character was not found.
    case Nil =>
      throw Error(s"$char not found")
    case (key, bits) :: tail =>
      // If the key (character) matches the desired character, return the associated bit sequence.
      if key == char then
        bits
      // Otherwise, recursively search for the character in the remaining code table entries.
      else
        codeBits(tail)(char)

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   *
   * @param tree The code tree to convert into a code table.
   * @return The resulting code table.
   */
  def convert(tree: CodeTree): CodeTable =
    /**
     * Inner recursive function that performs the conversion process.
     *
     * @param tree The current code tree node.
     * @param acc  The accumulated bit sequence.
     * @return The resulting code table.
     */
    def convertAcc(tree: CodeTree, acc: List[Bit]): CodeTable = tree match
      // If the node is a leaf, add the character and its associated bit sequence to the code table.
      case Leaf(char, _) =>
        List((char, acc))
      case Fork(left, right, _, _) =>
        mergeCodeTables(
          // Recursively convert the left subtree, adding a '0' bit to the accumulated sequence.
          convertAcc(left, acc ::: List(0)),
          // Recursively convert the right subtree, adding a '1' bit to the accumulated sequence.
          convertAcc(right, acc ::: List(1))
        )

    // Start the conversion process with an empty accumulator.
    convertAcc(tree, Nil) // Start the conversion process with an empty accumulator.

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   *
   * @param a The first code table.
   * @param b The second code table.
   * @return The merged code table.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   *
   * @param tree The code tree.
   * @param text The text to be encoded.
   * @return The encoded sequence of bits.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text match
    // Encode the first character and recursively encode the remaining text.
    case c :: cs =>
      codeBits(convert(tree))(c) ::: quickEncode(tree)(cs)
    // If there is no more text, return an empty sequence of bits.
    case Nil =>
      Nil

/**
 * This object serves as the entry point to the Huffman coding functionality.
 * It extends the `Huffman` class to inherit its methods and provides additional
 * utility functions for encoding and decoding.
 */
object Huffman extends Huffman
