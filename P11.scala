/** P11 - Modified run-length encoding
eg.
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
import P09.pack

object P11{
  def encodeModified[A](ls: List[A]): List[Any] = {
    pack(ls).map(x=> if (x.length == 1) x.head else (x.length, x.head))
  }
}
