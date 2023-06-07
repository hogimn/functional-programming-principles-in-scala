package funsets

/**
 * The entry point of the program.
 * It extends the `App` trait, allowing the code inside the object to be executed as a standalone application.
 */
object Main extends App:
  import FunSets.*
  // Prints whether the set contains the specified element.
  println(contains(singletonSet(1), 1))
