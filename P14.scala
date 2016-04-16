/** P14 - Duplicate the elements of a list.
eg.
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
*/
object P14{
  def duplicate[A](ls: List[A]): List[A] = {
    def duplicateInternal(ls: List[A], acc: List[A] = Nil):List[A] = ls match {
      case Nil => acc
      case h::t => duplicateInternal(t, acc ++ List.tabulate(2)(_ => h))
    }
    duplicateInternal(ls)
  }
  def duplicateSolution[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }
}
