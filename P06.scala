/*P06- is List Palindrome*/
object P06{
  def length[A](ls: List[A]):Int = {
    ls.foldLeft(0)((c,_) => c + 1)
  }
  def isPalindrome[A](lst: List[A]):Boolean = lst match {
    case h::Nil => true
    case h::t =>if(h==t.last) isPalindrome(t.dropRight(1))else false
    case Nil => true
  }

  def isPal[A](m:Int,
    isEven: Boolean,
    lst1:List[A],
    lst2: List[A] = Nil):Boolean = (m, lst1) match {
    case (0, h1::t1) => if (isEven) lst1 == lst2 else t1 == lst2
    case (k, h::t) => isPal(k-1,isEven, t, h::lst2)
    case _ => true
  }

  def isPalindromeRunner[A](lst: List[A]):Boolean = {
    val n = length(lst)
    val isEven = n%2 == 0
    val m = n/2
    isPal(m,isEven,lst)
  }
}
