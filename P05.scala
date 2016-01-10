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

  def reversePureFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()){(r,h) => h::r}
    // Nil (1,2,3,4,5)
    // (1,Nil) (2,3,4,5)
    // (2,1,Nil) (3,4,5)
    // (3,2,1,Nil) (4,5)
    // (4,3,2,1,Nil) (5)
    // (5,4,3,2,1) Nil ==> (5,4,3,2,1)
}
