/** P17 - Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:
scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
*/
object P17{
  def split[A](n: Int, ls: List[A]): (List[A],List[A]) = n match {
    case x if x >=0 => (ls.take(n), ls.drop(n))
    case _ => println("n has to be non-zero. Returning original list")
              (Nil, ls)
  }
}
