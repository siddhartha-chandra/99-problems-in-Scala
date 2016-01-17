/*P07- Flatten a nested List*/
object P07{
  def appendListList1To2[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
    case Nil => lst2
    case h::t => appendListList1To2(t, lst2 ::: List(h))
  }

  def flattenList[A](lst: List[List[A]], res: List[A] = Nil):List[A] = lst match {
    case Nil => res
    case h::t => flattenList(t, appendListList1To2(h, res))
  }
}
