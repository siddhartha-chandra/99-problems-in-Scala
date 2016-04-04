/** P13 - Run-length encoding of a list (direct solution)
eg.
scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/

object P13{
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    def encodeInternal[A](ls: List[A], acc: List[(Int,A)] = Nil): List[(Int, A)] = ls match {
      case Nil => acc
      case h::t => encodeInternal( t.dropWhile(_ == h), acc ++ List((t.takeWhile(_ == h).length + 1, h)))
    }
    encodeInternal(ls)
  }

  def encodeDirectSolution[A](ls: List[A]): List[(Int, A)] =
   if (ls.isEmpty) Nil
   else {
     val (packed, next) = ls span { _ == ls.head }
     (packed.length, packed.head) :: encodeDirect(next)
   }
}
