/** P16 - Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
*/
object P16{
  def drop[A](n: Int, ls: List[A]): List[A] = n match {
    case x if x>0 => ls match {
      case Nil => ls
      case h::t => (if (ls.length>= n) ls.take(n).dropRight(1) else ls.take(n)) ++ drop(n, ls.drop(n))
    }
    case _ => println("Invalid value for n. Returning original list")
            ls
  }

  def dropInternal[A](n: Int, ls: List[A], acc: List[A] = Nil): List[A] = ls match {
      case Nil => acc
      case h::t =>  dropInternal(n, ls.drop(n), acc ++ (if(ls.length>= n) ls.take(n).dropRight(1) else ls.take(n)) )
    }
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = n match {
      case x if x > 0 => dropInternal(n, ls)
      case _ => println("Invalid value for n. Returning original list")
                ls
  }

  // Functional...Implemented Solution .
  def dropFunctional[A](n: Int, ls: List[A]): List[A] = n match {
    case x if x> 0 => ls.zipWithIndex.filter{ v => (v._2 + 1) % n != 0 }.map(_._1)
    case _ => println("Invalid value for n. Returning original list")
              ls
  }
}
