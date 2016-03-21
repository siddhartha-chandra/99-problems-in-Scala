/** P10 - Run-length encoding of a list
eg.
scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/
import P09.pack

object P10{
  //my tail-recursive implementation
  def encode[A](ls: List[A]): List[(Int, A)] = {
    def encodeInternal[A](ls: List[A], res: List[(Int,A)] = Nil): List[(Int,A)] = ls match {
      case Nil => res
      case h::t => t match {
        case Nil =>
          println("Single element list")
          res ++ List((1,h))
        case _ =>
          val (pack, next) = ls span (_ == h)
          println(s"pack -> $pack")
          println(s"next -> $next")
          println("* ---- *")
          val count = pack.length
          encodeInternal(next, res ++ List((count, h)) )
      }
    }
    encodeInternal(ls)
  }

  def encodeUsingPack[A](ls: List[A]): List[(Int,A)] = {
    pack(ls).map(x=> (x.length, x.head))
  }
}
