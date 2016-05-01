/** P25 - Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.
Example:
scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
*/
import P23._
object P25{
  def randomPermute[A](ls: List[A]):List[A] = randomSelect(ls.length, ls)
}
