/*P08- Eliminate consecutive duplicates of list elements*/
object P08{
  //Standard recursive
  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h::t => h::compressRecursive(t.dropWhile(_ == h))
  }

  //tail recursive
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressInner[A](ls: List[A], aggregator: List[A] = List()): List[A] =ls match {
      case Nil => aggregator
      case h::t => compressInner(t.dropWhile(_ == h), aggregator :+ h)
    }
    compressInner(ls)
  }

  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()){
    (x,y) =>
      if(y.isEmpty || y.head != x){
        println(s"cond 1: $x --> $y")
        x::y
      }
      else {
        println(s"cond 2: $y")
        y
      }
    }


  def compress[A](ls: List[A]): List[A] = {
    def compressInner[A](ls: List[A], aggregator: List[A] = List()): List[A] =ls match {
      case Nil => aggregator
      case h::Nil => aggregator :+ h
      case h::t => if (h==t.head) compressInner(t, aggregator) else compressInner(t, aggregator :+ h)
    }
    compressInner(ls)
  }
}
