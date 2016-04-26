/** P21 - Insert an element at a given position into a list.
Example:
scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
*/
object P21{
  def insertAt[A](elem: A, n: Int, ls: List[A]): List[A] = n match {
    case x if x>=0 && x<=ls.length => ls.take(x) ++ List(elem) ++ ls.drop(x)
    case _ => throw new Exception("n does not lie in the given list")
  }
}
