/** P12 - Decode a run-length encoded list
eg.
scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
*/

object P12{
  def decode[A](ls: List[(Int, A)]): List[A] = {
    def decodeInternal(ls: List[(Int, A)], acc: List[A] = Nil): List[A] = ls match {
      case Nil => acc
      case h :: t => decodeInternal(t, acc ++ List.tabulate(h._1)(_ => h._2))
    }
    decodeInternal(ls)
  }

  // def decodeSolution[A](ls: List[(Int, A)]): List[A] =
  //   ls flatMap { e => List.make(e._1, e._2) }
}
