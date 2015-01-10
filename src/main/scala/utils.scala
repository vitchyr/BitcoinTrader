// Utility functions
package object utils {
  // in milliseconds
  def time(): Long = System.currentTimeMillis

  def removeDuplicates[A](xs: List[A]): List[A] =
    if (xs.isEmpty) xs
    else xs.head :: removeDuplicates(xs.tail filter (x => x != xs.head))

  def sumList(lst1: List[Double], lst2: List[Double]): List[Double] =
    (lst1 zip lst2) map { case (d1, d2) => d1 + d2 }

  def scaleList(lst: List[Double], factor: Double): List[Double] =
    lst map (d => factor * d)
}
