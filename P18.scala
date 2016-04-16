/** P18 - Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:
scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
*/
object P18{
  def sliceTailRecursiveInternal[A](sliceBegin: Int, sliceEnd: Int, ls: List[A], acc: List[A] = Nil): List[A] = (sliceBegin, sliceEnd, ls) match {
    case (0, 0, h::t) => acc
    case (0, b, h::t) => sliceTailRecursiveInternal(0, b-1, t, acc :+ h)
    case (a, b, h::t) => sliceTailRecursiveInternal(a-1, b-1, t, acc)
    case _ => acc
  }
  def sliceTailRecursive[A](sliceBegin: Int, sliceEnd: Int, ls: List[A]): List[A] = {
    (sliceBegin,sliceEnd) match{
      case (a, b) if a>= 0 && a <= b => sliceTailRecursiveInternal(sliceBegin, sliceEnd, ls)
      case _ =>  println("Invalid input. Returning original list.")
              ls
      }
  }

  def sliceFunctional[A](sliceBegin: Int, sliceEnd: Int, ls: List[A]): List[A] = (sliceBegin,sliceEnd) match {
    case (a, b) if a>= 0 && a <= b => ls.drop(a).take(b-a)
    case _ =>  println("Invalid input. Returning original list.")
              ls
  }
}
