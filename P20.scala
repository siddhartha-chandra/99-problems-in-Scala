/** P20 - Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:
scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
*/
object P20{
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = n match {
    case x if x>=0 && x<ls.length => (ls.take(x) ++ ls.drop(x+1), ls.drop(x).head)
    case _ => throw new Exception("n does not lie in the given list")
  }
}
