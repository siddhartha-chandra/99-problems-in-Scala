/** P19 -Rotate a list N places to the left.
Examples:
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
*/
object P19{
  def rotateFunctional[A](n: Int, ls: List[A]): List[A] = n match {
    case x if x < 0 =>
      ls.drop(ls.length + x) ++ ls.dropRight(-x)
    case x if x > 0 =>
      ls.drop(x) ++ ls.take(x)
    case _ => ls
  }
}
