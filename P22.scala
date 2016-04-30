/** P22 - Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
*/
object P22{
  def range(begin: Int, end: Int): List[Int] = {
      if (begin == end) List(end)
      else if (begin<end) begin :: range(begin+1,end)
      else begin :: range(begin-1,end)
  }

  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
  f(s) match {
    case None         => Nil
    case Some((r, n)) => r :: unfoldRight(n)(f)
  }

  def rangeFunctional(begin: Int, end: Int): List[Int] =
    unfoldRight(begin) { n =>
      if (n > end) None
      else Some((n, n + 1))
  }
}
