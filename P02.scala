/*P02- Get Penultimate element of list*/

object P02{
  def getPenultimateElem[A](ls: List[A]):A = ls match {
      case head::Nil => throw new Exception("Only one element in list")
      case head::tail =>
          tail match {
            case h::Nil=> head
            case h::t => getPenultimateElem(tail)
            case _ => throw new Exception("Something weird happened!")
          }
      case _ => throw new NoSuchElementException
  }

  def getPenultimateElemElegant[A](ls: List[A]):A = ls match {
      case h::_::Nil => h
      case _::tail => getPenultimateElemElegant(tail)
      case _ => throw new NoSuchElementException
  }

}
