/*P04 - Find number of elements in a list*/

object P04{
  def lengthRecursive[A](ls: List[A]):Int = ls match {
    case Nil => 0
    case _::t => 1 + lengthRecursive(t)
  }

  def lengthTailRecursive[A](ls: List[A]):Int = {
    def lengthR(result: Int, curList: List[A]): Int =
      curList match{
        case Nil => result
        case _::t => lengthR(result+1, t)
      }
      lengthR(0, ls)
  }

  def lengthFunctional[A](ls: List[A]):Int = {
    ls.foldLeft(0)((c,_) => c + 1)
  }
}
