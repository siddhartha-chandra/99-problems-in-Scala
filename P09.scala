/*P09- Pack consecutive duplicates of list elements into sublists */

/**
eg.
in: pack(List(1,1,1,1,2,2,2,5,6,7,7,7))
out: List(List(1,1,1,1), List(2,2,2), List(5), List(6), List(7))
*/

object P09{
  //My tail-recursive implementation
  def pack[A](ls: List[A]): List[List[A]] = {
    def packInternal(ls: List[A], finalList: List[List[A]] = List[List[A]]()): List[List[A]] = ls match {
        case Nil => finalList
        case h::t =>  packInternal(t.dropWhile(_ == h),finalList ++ List(h +: t.takeWhile(_ == h)))
    }
    packInternal(ls)
  }

  //my functional implementation
  def packFunctional[A](ls: List[A]): List[List[A]] = {
    ls.foldRight(List[List[A]]()){
      (x,y) => if (y.isEmpty || y.head.head != x)
                  List(x) :: y
               else
                  (x :: y.head) :: y.tail
    }
  }

  //Given Solution
  def packSol[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: packSol(next)
      }
  }

}
