/** P24 - Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
*/
import P23._
object P24{
  def lotto(n: Int, total: Int):List[Int] = (n,total) match {
    case (a, b) if a <= b && a > 0 =>
      val ls = List.range(1,total + 1)
      randomSelect(a, ls)

    case _ => throw new Exception("Invalid case!")
  }
}
