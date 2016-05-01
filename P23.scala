/** P23 - Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
*/
import P20.removeAt

object P23{
  def randomSelectInternal[A](n: Int, ls: List[A], agg: List[A] = Nil):List[A] = n match {
    case 0 => agg
    case _ =>
      val rand = (scala.util.Random).nextInt(ls.length)
      val (nextList, elem) = removeAt(rand, ls)
      randomSelectInternal(n-1, nextList, agg :+ elem)
  }

  def randomSelect[A](n: Int, ls: List[A]):List[A] = n match {
    case x if x > ls.length || x < 0 =>
      throw new Exception("n is less than 0 or greater than number of elements in List ")
    case _ => randomSelectInternal(n, ls)
  }
}
