/*P05 - Reverse a list*/

object P05{
  def reverse[A](ls: List[A]):List[A] = ls match {
    case h::t => reverse(t) ::: List(h)
    case Nil => ls
  }

  def reverseTailRecursive[A](ls: List[A]):List[A] = {
    def reverseTail[A](result: List[A], curList: List[A]): List[A] = curList match {
      case h::t => reverseTail(h::result,t)
      case Nil => result
    }
    reverseTail(Nil, ls)
  }
}
