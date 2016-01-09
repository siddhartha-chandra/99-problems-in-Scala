/*P03 - Find Kth element in a list*/

object P03{
  def findKthElement[A](k: Int, ls: List[A]):A = (k,ls) match {
    case (0, h::_) => h
    case (k, _::tail) => findKthElement(k-1,tail)
    case (k,_) if k<0 =>
      throw new Exception("Please specify a positive value for k")
    case _ => throw new NoSuchElementException
  }

  def findKthElementElegant[A](k: Int, ls: List[A]):A = (k,ls) match {
    case (0, h::_) => h
    case (k, _::tail) => findKthElement(k-1,tail)
    case (_, Nil) => throw new NoSuchElementException
  }
}
