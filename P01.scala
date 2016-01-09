/*P01  - Find last element in List*/

object P01 {
  def lastRecursive[A](ls: List[A]): A = ls match {
    case head::Nil => head
    case head::tail => lastRecursive(tail)
    case _ => throw new NoSuchElementException
  }
}
