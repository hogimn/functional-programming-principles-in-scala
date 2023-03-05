package recfun

import scala.annotation.tailrec

/**
 * The `RecFun` object serves as the entry point for the recursive function exercises.
 * It extends the `RecFunInterface` which provides the interface for the exercise functions.
 */
object RecFun extends RecFunInterface:

  /**
   * Main method to execute the program.
   * It prints Pascal's Triangle.
   */
  def main(args: Array[String]): Unit =
    // Print the header for Pascal's Triangle
    println("Pascal's Triangle")
    // Iterate over each row from 0 to 10
    for row <- 0 to 10 do
      // Iterate over each column from 0 to the current row
      for col <- 0 to row do
        // Iterate over each column from 0 to the current row
        print(s"${pascal(col, row)} ")
      // Move to the next line after printing the row
      println()

  /**
   * Exercise 1: Pascal's Triangle
   * Computes the value of a specific cell in Pascal's Triangle.
   *
   * @param c The column index of the cell.
   * @param r The row index of the cell.
   * @return The value at the specified cell.
   */
  def pascal(c: Int, r: Int): Int =
  // Base case: first or last column of a row
    if c == 0 || c == r then
      1
    // Recursive case: sum of two values from the previous row
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2: Balanced Parentheses
   * Checks if the given list of characters has balanced parentheses.
   *
   * @param chars The list of characters to check.
   * @return True if parentheses are balanced, false otherwise.
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balanceAcc(chars: List[Char], acc: Int): Int =
      // Return the accumulated value (balance count)
      if chars.isEmpty || acc < 0 then
        acc
      // Increment the count for an opening parenthesis
      else if chars.head == '(' then
        balanceAcc(chars.tail, acc + 1)
      // Decrement the count for a closing parenthesis
      else if chars.head == ')' then
        balanceAcc(chars.tail, acc - 1)
      // Ignore other characters and continue iteration
      else
        balanceAcc(chars.tail, acc)

    // Check if the final count is zero, indicating balanced parentheses
    balanceAcc(chars, 0) == 0

  /**
   * Exercise 3: Counting Change
   * Computes the number of ways to make change for a given amount of money using a list of coins.
   *
   * @param money The amount of money to make change for.
   * @param coins The list of coin denominations.
   * @return The number of ways to make change.
   */
  def countChange(money: Int, coins: List[Int]): Int =
    // Base case: Exact change reached
    if money == 0 then
      1
    // Base case: No valid solution possible
    else if money < 0 || coins.isEmpty then
      0
    // Recursive case: Explore all possibilities by including or excluding a coin
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
