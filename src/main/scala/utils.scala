// Utility functions
package object utils {
  // returns the time in milliseconds
  def time(): Long = System.currentTimeMillis

  /* Return [xs] with duplicates removed. Order is maintained. If there are
   * duplicates, the first copy is kept. */
  def removeDuplicates[A](xs: List[A]): List[A] =
    if (xs.isEmpty) xs
    else xs.head :: removeDuplicates(xs.tail filter (x => x != xs.head))

  // Returns the pairwise addition of two lists
  def sumList(lst1: List[Double], lst2: List[Double]): List[Double] =
    (lst1 zip lst2) map { case (d1, d2) => d1 + d2 }

  // Scale each value in [lst] by [factor]
  def scaleList(lst: List[Double], factor: Double): List[Double] =
    lst map (d => factor * d)
}
