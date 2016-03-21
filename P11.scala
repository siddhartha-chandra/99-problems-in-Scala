/** P11 - Modified run-length encoding
eg.
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
import P10.encodeUsingPack

object P11{
  def encodeModified[A](ls: List[A]): List[Any] = {
    encodeUsingPack(ls).map(x=> if (x._1 == 1) x._1 else x)
  }

  def encodeModifiedTypesafe[A](ls: List[A]): List[Either[A, (Int, A)]] = {
    encodeUsingPack(ls).map(x=> if (x._1 == 1) Left(x._2) else Right(x))
  }
}
