/*P08- Eliminate consecutive duplicates of list elements*/
object P08{
  def compress[A](ls: List[A]): List[A] = {
    def compressInner[A](ls: List[A], aggregator: List[A] = List()): List[A] =ls match {
      case Nil => aggregator
      case h::Nil => aggregator :+ h
      case h::t => if (h==t.head) compressInner(t, aggregator) else compressInner(t, aggregator :+ h)
    }
    compressInner(ls)
  }
}
